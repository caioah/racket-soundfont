#lang racket
(require binary-class/riff binary-class/base binary-class/common binary-class/string
         (for-syntax (only-in racket/list append-map)))
(provide (except-out (all-defined-out) silence))
(define generator-enum
  '(startAddrsOffset ;smpls
    endAddrsOffset ;smpls
    startloopAddrsOffset ;smpls
    endloopAddrsOffset ;smpls
    startAddrsCoarseOffset ;smpls*32768
    modLfoToPitch ;cent fs
    vibLfoToPitch ;cent fs
    modEnvToPitch ;cent fs
    initialFilterFc ;cent
    initialFilterQ ;cB
    modLfoToFilterFc ;cent fs
    modEnvToFilterFc ;cent fs
    endAddrsCoarseOffset ;smpls*32768
    modLfoToVolume ;cb
    undefined ;14
    chorusEffectsSend ;1/1000
    reverbEffectsSend ;1/1000
    pan ;1/500
    undefined ;18
    undefined ;19
    undefined ;20
    delayModLFO ;timecent
    freqModLFO ;cent
    delayVibLFO ;timecent
    freqVibLFO ;cent
    delayModEnv ;timecent
    attackModEnv ;timecent
    holdModEnv ;timecent
    decayModEnv ;timecent
    sustainModEnv ;1/1000
    releaseModEnv ;timecent
    keynumToModEnvHold ;tcent/key
    keynumToModEnvDecay ;tcent/key
    delayVolEnv ;timecent
    attackVolEnv ;timecent
    holdVolEnv ;timecent
    decayVolEnv ;timecent
    sustainVolEnv ;cB attn
    releaseVolEnv ;timecent
    keynumToVolEnvHold ;tcent/key
    keynumToVolEnvDecay ;tcent/key
    instrument ;index
    undefined ;42
    keyRange ;midi key
    velRange ;midi vel
    startloopAddrsCoarseOffset ;smpls*32768
    keynum ;midi key
    velocity ;midi vel
    initialAttenuation ;cB
    undefined ;49
    endloopAddrsCoarseOffset  ;smpls*32768
    coarseTune ;semitone
    fineTune ;cent
    sampleID ;index
    sampleModes ;bit flags
    undefined ;55
    scaleTuning ;cent/key
    exclusiveClass ;arbitrary
    overridingRootKey ;midi key
    undefined ;59
    endOper))
(define sample-type
  '(mono right left linked rom-mono rom-right rom-left rom-linked)) ;(1 2 4 8 32769 32770 32772 32776)
(define mod-trans '(linear absolute)) ;(0 2)
(define mod-source
  '(no-controller note-on-vel note-on-key poly-pressure channel-pressure pitch-wheel
    pitch-wheel-sens link)) ; 0,2,3,10,13,14,16,127
(define mod-type '(linear concave convex switch))
(struct soundsample (name type rate pitch correction data link big?) #:transparent)
(define (silence len)
  (soundsample "silence" 1 #f #f 0 (open-input-bytes #"") #f #f))
(define (recover-string s)
  (string-trim (car (regexp-match #rx"^[^\u0000]+" s))))
(define bytes->string-downcase (compose1 string-downcase bytes->string/utf-8))
(define-syntax (named-hash stx)
  (syntax-case stx ()
    [(_ sym ...)
     (with-syntax ([(kp ...) (append-map (λ (s) (list #`'#,s s)) (syntax-e #'(sym ...)))])
       #'(hash kp ...))]))

(define-binary-class phdr% 
  ([name (iso-8859-1-string 20)] [preset l2] [bank l2] [bag l2]
   [library l4] [genre l4] [morphology l4])
  (define/public (get-hash)
    (let ([name (recover-string name)])
      (named-hash name preset bank bag library genre morphology))))
(define-binary-class shdr% 
  ([name (iso-8859-1-string 20)] [start l4] [end l4]
   [start-loop l4] [end-loop l4] [rate l4] [pitch u1]
   [correction u1] [link l2] [type l2])
  (define/public (get-hash)
    (let ([name (recover-string name)])
      (named-hash name start end start-loop end-loop rate pitch correction link type))))
(define-binary-class sf% riff% ()
  #:dispatch 
  (if (member id '(#"LIST" #"RIFF") bytes=?)
      sf-list% sf-rec%))
(define-binary-class sf-list% sf%
  ([type fourcc])
  #:dispatch
  (and (writeln type)
       (case type
         [(#"INFO") info%]
         [(#"sdta") sdta%]
         [(#"pdta") pdta%]
         [else soundfont-file%])))
(define-binary-class sf-riff% sf-list%
  ([chunks (sf-list riff:chunk% (- size 4))])
  (define/public (chunk-ref x)
    (let* ([x (bytes->string-downcase x)]
           [v (findf (λ (c) (string=? (bytes->string-downcase (get-field id c)) x)) chunks)])
      (and v (get-field data v))))
  (define/public (get-ids)
    (map (λ (c) (get-field id c)) chunks)))
;SDTA
(define-binary-class sdta% sf-riff% ()
  (inherit get-ids chunk-ref)
  (define/public (bytes-ref start end)
    (subbytes (chunk-ref #"smpl") (* 2 start) (* 2 end)))
  (define (loop-sample start end len)
    (let-values ([(s) (bytes-ref start end)]
                 [(q r) (quotient/remainder (- end start) len)])
      (apply bytes-append
             (append (make-list q s)
                     (list (subbytes s 0 (* 2 (- len r))))))))
  (define/public (sample-bytes start end [len #f] [start-loop #f] [end-loop #f])
    (open-input-bytes
    (cond [(not len) (bytes-ref start end)]
          [(= len 0) #""]
          [(not start-loop) (loop-sample start end len)]
          [(not end-loop)
           (if (<= len (- end start))
               (bytes-ref start len)
               (bytes-append (bytes-ref start start-loop)
                (loop-sample start-loop (- end 8)
                            (- len 8 (- start-loop start)))
                (bytes-ref (- end 8) end)))]
          [else
           (if (<= (- len (- end end-loop)) (- end-loop start))
               (bytes-ref start len)
               (bytes-append (bytes-ref start start-loop)
                (loop-sample start-loop end-loop
                            (- len (- start-loop start) (- end end-loop)))
                (bytes-ref end-loop end)))]))))
(define-binary-class pdta% sf-list%
  ([phdr sf-rec%] [pbag sf-rec%] [pmod sf-rec%] [pgen sf-rec%]
   [inst sf-rec%] [ibag sf-rec%] [imod sf-rec%] [igen sf-rec%]
   [shdr sf-rec%])
  (define/public (count) (sub1 (length (get-field data phdr))))
  (define (get-modgen-list bag mods gens b1 b2)
    (let ([bx (build-list (- b2 b1)
                          (λ (i) (list-ref (get-field data bag) (+ b1 i))))])
    (for/fold ([lst '()]) ([b1 (in-range (sub1 (length bx)))] [b2 (in-range 1 (length bx))])
      (let ([mi (get-field mod (list-ref bx b1))]
            [gi (get-field gen (list-ref bx b1))])
      (append lst (list (cons (let ([m (list-ref (get-field data mods) mi)])
                                (if (empty? m) m (send m get-hash)))
                              (build-list (- (get-field gen (list-ref bx b2)) gi)
                                          (λ (i) (let ([g (list-ref (get-field data gens) (+ gi i))])
                                                   (if (empty? g) g (send g get-oper))))))))))))
  (define/public (preset-count) (sub1 (length (get-field data phdr))))
  (define/public (preset-ref k)
    (let ([h1 (list-ref (get-field data phdr) k)]
          [h2 (list-ref (get-field data phdr) (add1 k))])
      (cons (send h1 get-hash)
            (get-modgen-list pbag pmod pgen (get-field bag h1) (get-field bag h2)))))
  (define/public (instrument-count) (sub1 (length (get-field data inst))))
  (define/public (instrument-ref k)
    (let ([h1 (list-ref (get-field data inst) k)]
          [h2 (list-ref (get-field data inst) (add1 k))])
      (cons (recover-string (get-field name h1))
            (get-modgen-list ibag imod igen (get-field bag h1) (get-field bag h2)))))
  (define/public (sample-count) (length (get-field data shdr)))
  (define/public (sample-ref k)
    (send (list-ref (get-field data shdr) k) get-hash)))

(define-binary-class info% sf-riff% ()
  (inherit get-ids chunk-ref)
  (define/public (get-hash)
    (for/fold ([h (hash)])
              ([k (in-list (get-ids))])
      (let ([s (chunk-ref k)])
        (if s (hash-set h k s) h)))))
(define-binary-class soundfont-file% sf-list%
  ([info info%] [sdta sdta%] [pdta pdta%])
  
  (define/public (sample-ref k [len #f] [start-off 0] [end-off 0] [start-loop-off 0] [end-loop-off 0] [big? #f])
    (let ([s (send pdta sample-ref k)])
      (soundsample (string-trim (second (regexp-match #rx"^([^\u0000]+)" (hash-ref s 'name))))
       (hash-ref s 'type) (hash-ref s 'rate)
       (hash-ref s 'pitch) (hash-ref s 'correction)
       (send sdta sample-bytes (+ (hash-ref s 'start) start-off)
             (+ (hash-ref s 'end) end-off) len
             (+ (hash-ref s 'start-loop) start-loop-off)
             (+ (hash-ref s 'end-loop) end-loop-off))
       (case (bitwise-and (hash-ref s 'type) 15)
         [(8 9 10 12)
          (sample-ref (hash-ref s 'link) len start-off end-off start-loop-off end-loop-off big?)]
         [else #f]) big?)))
  (define/public (instrument-range k)
    (let* ([i (send pdta instrument-ref k)]
           [g (and (not (empty? (third i)))
                   (map (λ (lst) (map (λ (x) (send x get-oper)) lst)) (third i)))]
           [rng (and g (filter (λ (r) (and (not (empty? r)) (eq? (first (car r)) 'keyRange))) g))])
      (and rng (cons (second (if (eq? (caaar rng) 'keyRange) (caar rng) (caadr rng)))
                     (third (car (last rng)))))))
  (define (get-offset rng coarse addrs)
    ((λ (x y) (and y (+ (second y) (* 32768 (if x (second x) 0)))))
     (findf (λ (v) (eq? (car v) coarse)) rng)
     (findf (λ (v) (eq? (car v) addrs)) rng)))
  (define (keyrange-sample rng len [big? #f])
    (sample-ref (second (findf (λ (v) (eq? (car v) 'sampleID)) rng)) len
                (get-offset rng 'startAddrsCoarseOffset 'startAddrsOffset)
                (get-offset rng 'endAddrsCoarseOffset 'endAddrsOffset)
                (get-offset rng 'startloopAddrsCoarseOffset 'startloopAddrsOffset)
                (get-offset rng 'endloopAddrsCoarseOffset 'endloopAddrsOffset))))

(define-binary-class inst% 
  ([name (iso-8859-1-string 20)] [bag l2]))
(define-binary-class pbag% 
  ([gen l2] [mod l2]))
(define-binary-class ibag% pbag% ())
(define-binary-class pmod% 
  ([src l2] [dest l2] [amt l2] [amt-src l2] [trans l2])
  (define/public (get-hash)
    (hash 'type (list-ref mod-type (arithmetic-shift src -9))
          'index (list-ref mod-source (index-of '(0 2 3 10 13 14 16 127) (bitwise-and src 63) =))
          'polarity (= 512 (bitwise-and src 512))
          'direction (= 256 (bitwise-and src 256))
          'cc (= 128 (bitwise-and src 128))
          'transform (list-ref mod-trans (index-of '(0 2) trans =))
          'amount amt
          'source-amount amt-src
          'gen (list-ref generator-enum dest))))
(define-binary-class pgen% 
  ([code l2] [amt (bytestring 2)])
  (define/public (get-oper [signed? #f] [big? #f])
    (let ([x (list-ref generator-enum code)]
          [scale 1] [signed #t])
      (case x
        [(chorusEffectsSend reverbEffectsSend sustainVolEnv)
         (set! scale 1/1000) (set! signed #f)]
        [(pan) (set! scale 1/500)]
        [(sustainModEnv) (set! scale -1/1000) (set! signed #f)]
        [(initialFilterQ modLfoToPitch vibLfoToPitch modEnvToPitch modLfoToFilterFc modEnvToFilterFc
          modLfoToVolume sustainVolEnv keyRange velRange keynum velocity initialAttenuation coarseTune
          fineTune sampleModes scaleTuning exclusiveClass overridingRootKey)
         (set! signed #f)]
        [else (void)])
      (list x (* (integer-bytes->integer amt signed big?) scale)))))
(define-binary-class imod% pmod% ())
(define-binary-class igen% pgen% ())

(define (sf-list type size)
  (binary (λ (in) (define end (+ (file-position in) size))
            (let loop ()
              (define p (file-position in))
              (cond [(> p end) (error "Invalid soundfont chunk")]
                    [(= p end) '()]
                    [else (cons (read-value type in) (loop))])))
          (λ (out lst) (for ([v (in-list lst)])
                         (send v write out)))))
(define-binary-class sf-rec% sf%
  ([data (case id
           [(#"phdr") (sf-list phdr% size)]
           [(#"shdr") (sf-list shdr% size)]
           [(#"inst") (sf-list inst% size)]
           [(#"pbag") (sf-list pbag% size)]
           [(#"ibag") (sf-list ibag% size)]
           [(#"pmod") (sf-list pmod% size)]
           [(#"imod") (sf-list imod% size)]
           [(#"pgen") (sf-list pgen% size)]
           [(#"igen") (sf-list igen% size)]
           [else (pad/word (bytestring size))])]))





                      
    
    
  