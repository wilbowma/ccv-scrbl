#lang scribble/base
@(require
  scribble/core
  racket/date
  racket/dict
  racket/contract)

@(define paper-type/c (or/c 'paper 'poster 'abstract))
@(define proceedings-string/c (and/c string? (lambda (x) (regexp-match? #px"Proceedings of" x))))
@(define publish-status/c (or/c 'accepted 'in-press 'published 'revision-requested 'submitted))
@(define year/c (between/c 1930 (add1 (date-year (current-date)))))
@(define month/c (between/c 1 12))
@(define role/c (or/c 'chair 'co-chair 'committee-member 'ex-officio 'cgroup-chair))
@(define org-type/c (or/c 'academic 'federal 'municipal 'provincal 'private 'non-profit))

@;; need multimap abstraction
@;; should put it on the package server

@(struct none ())
@(define None (none))

@(define (multimap-set! map key value)
  (set-box! map (cons (cons key value) (unbox map))))

@(define (make-multimap) (box '()))
@(define multimap? (box/c (listof (cons/c any/c any/c))))
@(define (multimap-length mm) (length (unbox mm)))

@(define/contract (dict-filter dict f)
   (-> dict? procedure? list?)
   (filter (lambda (x)
             (f (car x) (cdr x)))
           (dict->list dict)))

@(define/contract (dict->table dict)
   (-> dict? table?)
   (tabular
    #:sep (hspace 2)
    #:column-properties '(left left)
    #:row-properties '(bottom-border top-border)
    #:cell-properties '((top))
    (map (lambda (x) (list (car x) (format "~a" (cdr x)))) (dict-filter dict (lambda (k v) (not (equal? v None)))))))

@(define conf-papers (make-multimap))

@(define (bool->string b) (if b "Yes" "No"))
@(define (author-ls->string authors)
   (for/fold ([s ""])
             ([a authors])
     (string-append
      s
      (format "~a~a"
              (if (not (equal? "" s)) ", " " ")
              a))))

@;; Actually, this wants a multimap of dicts
@(define/contract (multimap->itemlist mm #:sort [lt <])
   (->i ([mm multimap?])
        (#:sort [lt procedure?])
        [ils itemization?]
        #:post (mm ils)
        (= (length (itemization-blockss ils)) (multimap-length mm)))
   @itemlist[#:style 'ordered
             (for/list ([e (sort (unbox mm) lt #:key car)])
               (item
                (dict->table (cdr e))
                (linebreak)))
             ])

@(define pre-sidechannel 0)
@;; need a better define/contract abstraction; this is fcking stupid
@(define/contract (paper
                   #:title title
                   #:type type
                   #:status status
                   #:year year
                   #:conference conf
                   #:country country
                   #:city city
                   #:referred ref?
                   #:invited invited
                   #:authors authors
                   #:proceedings [proc #f]
                   #:page-first [pg1 #f]
                   #:page-end [pgn #f])
   (->i (#:title [title string?]
         #:type [type paper-type/c]
         #:year [year year/c]
         #:status [status publish-status/c]
         #:conference [conf string?]
         #:country [country string?]
         #:city [city string?]
         #:referred [ref? boolean?]
         #:invited [invited? boolean?]
         #:authors [authors (listof string?)])
        (#:page-first [pg1 (or/c natural-number/c #f)]
         #:page-end [pgn (or/c natural-number/c #f)]
         #:proceedings [proc (or/c proceedings-string/c #f)])
        #:pre ()
        (set! pre-sidechannel (multimap-length conf-papers))
        #:pre/name (pg1 pgn)
        "Must provide both a first and end page, or neither"
        (or (and pg1 pgn) (and (not pg1) (not pgn)))
        #:pre/name (proc type)
        "Must provide a proceedings for papers"
        (if (eq? type 'paper) proc #t)
        [_ any/c]
        #:post/name ()
        "Expected the conf-papers to grow, but it didn't"
        (> (multimap-length conf-papers) pre-sidechannel))
   (multimap-set!
    conf-papers
    year
    (list (cons "Paper Title" title)
          (cons "Published In" (or proc None))
          (cons "Page Range" (if pg1 (format "~a--~a" pg1 pgn) None))
          (cons "Published Status" status)
          (cons "Year" year)
          (cons "Conference Name" conf)
          (cons "Conference Country"  country)
          (cons "Conference City" city)
          (cons "Referred" (bool->string ref?))
          (cons "Invited" (bool->string invited))
          (cons "Authors" (author-ls->string authors)))))

@(define presentations (make-multimap))

@(define/contract (presentation
                   #:title title
                   #:event event
                   #:country country
                   #:city city
                   #:year year
                   #:invited [invited? #f]
                   #:keynote [keynote? #f]
                   #:co-presenters [co-presenters #f])
   (->i (#:title [title string?]
         #:event [event string?]
         #:year [year year/c]
         #:country [country string?]
         #:city [city string?])
        (#:co-presenters [authors (or/c (listof string?) #f)]
         #:keynote [keynote? boolean?]
         #:invited [invited? boolean?])
        #:pre () (set! pre-sidechannel (multimap-length presentations))
        [_ any/c]
        #:post ()
        (= (multimap-length presentations) (add1 pre-sidechannel)))
   (multimap-set!
    presentations
    year
    (list
     (cons "Presentation Title" title)
     (cons "Event Name" event)
     (cons "Country" country)
     (cons "City" city)
     (cons "Invited" (bool->string invited?))
     (cons "Keynote" (bool->string keynote?))
     (cons "Year" year)
     (cons "Co-presenters" (if co-presenters
                               (author-ls->string co-presenters)
                               None)))))

@(define service-map (make-multimap))
@(define/contract (service
                   #:role role
                   #:name name
                   #:start-year syear
                   #:start-month smonth
                   #:end-year eyear
                   #:end-month emonth
                   #:org org
                   #:desc [desc #f])
   (->i (#:role [role role/c]
         #:name [name string?]
         #:start-year [syear year/c]
         #:start-month [smonth month/c]
         #:end-year [eyear year/c]
         #:end-month [emonth month/c]
         #:org [org string?])
        (#:desc [desc (or/c string? #f)])
        #:pre () (set! pre-sidechannel (multimap-length service-map))
        [_ any/c]
        #:post ()
        (= (multimap-length service-map) (add1 pre-sidechannel)))
   (multimap-set!
    service-map
    syear
    (list
     (cons "Role" role)
     (cons "Committee Name" name)
     (cons "Start Year" syear)
     (cons "Start Month" smonth)
     (cons "End Year" eyear)
     (cons "End Month" emonth)
     (cons "Organization" org)
     (cons "Description" (or desc None)))))

@(define position-map (make-multimap))

@(define/contract (academic-position
                   #:title title
                   #:type type
                   #:start-year syear
                   #:start-month smonth
                   #:org-name org
                   #:org-type org-type
                   #:tenure tenure
                   #:end-year [eyear #f]
                   #:end-month [emonth #f]
                   #:rank [rank #f]
                   #:desc [desc #f]
                   #:dept [dept #f]
                   #:school [school #f]
                   )
   (->i (#:title [title string?]
         #:type [type (or/c 'full-time 'part-time)]
         #:start-year [syear year/c]
         #:start-month [smonth month/c]
         #:org-name [org string?]
         #:org-type [org-type org-type/c]
         #:tenure [tenure (or/c 'non-tenure 'tenure-track 'tenured)])
        (#:rank [rank (or/c 'assistant 'associate 'lecturer 'professor 'emeritus #f)]
         #:end-year [eyear (or/c year/c #f)]
         #:end-month [emonth (or/c month/c #f)]
         #:desc [desc (or/c string? #f)]
         #:dept [dept (or/c string? #f)]
         #:school [school (or/c string? #f)])
        #:pre () (set! pre-sidechannel (multimap-length position-map))
        #:pre/name (eyear emonth)
        "Must specify both end year and end month, or neither"
        (or (and eyear emonth)
            (and (not eyear) (not emonth)))
        [_ any/c]
        #:post ()
        (= (multimap-length position-map) (add1 pre-sidechannel)))
   (multimap-set!
    position-map
    syear
    (list
     (cons "Position Title" title)
     (cons "Position Type" type)
     (cons "Academic Rank" (or rank None))
     (cons "Start Year" syear)
     (cons "Start Month" smonth)
     (cons "End Year" (or eyear None))
     (cons "End Month" (or emonth None))
     (cons "Organization Name" org)
     (cons "Organization Type" org-type)
     (cons "Department" (or dept None))
     (cons "Faculty / School / College" (or school None))
     (cons "Tenure Status" tenure)
     (cons "Work Description" (or desc None)))))

@(define recognition-map (make-multimap))

@(define/contract (recognition
                   #:type type
                   #:name name
                   #:org org
                   #:start-date start
                   #:amount [amount #f]
                   #:end-date [end #f]
                   #:desc [desc #f])
   (->i (#:type [type (or/c 'prize/award 'honor)]
         #:name [name string?]
         #:org [org string?]
         #:start-date [start date?])
        (#:amount [amount (or/c natural-number/c #f)]
         #:end-date [end (or/c date? #f)]
         #:desc [desc (or/c string? #f) ])
        #:pre () (set! pre-sidechannel (multimap-length recognition-map))
        [_ any/c]
        #:post ()
        (= (multimap-length recognition-map) (add1 pre-sidechannel)))
   (multimap-set!
    recognition-map
    (date-year start)
    (list
     (cons "Award Type" type)
     (cons "Award Name" name)
     (cons "Organization" org)
     (cons "Effective Date" (format "~a/~a" (date-year start) (date-month start)))
     (cons "End Date" (if end
                          (format "~a/~a" (date-year end) (date-month end))
                          None))
     (cons "Amount" (or amount None))
     (cons "Description" (or desc None)))))

@(define degrees-map (make-multimap))
@(define/contract (degree
                   #:type type
                   #:org org
                   #:start-date start
                   #:recv-date recv
                   #:supervisor [adviser #f]
                   #:special [spec #f])
   (->i (#:type [type string?]
         #:org [org string?]
         #:recv-date [recv date?]
         #:start-date [start date?])
        (#:special [spec (or/c string? #f)]
         #:supervisor [adviser (or/c string? #f)])
        #:pre () (set! pre-sidechannel (multimap-length degrees-map))
        [_ any/c]
        #:post ()
        (= (multimap-length degrees-map) (add1 pre-sidechannel)))
   (multimap-set!
    degrees-map
    (date-year recv)
    (list
     (cons "Degree Type" type)
     (cons "Organization" org)
     (cons "Specialization" (or spec None))
     (cons "Supervisor" (or adviser None))
     (cons "Degree Start Date" (format "~a/~a" (date-year start) (date-month start)))
     (cons "Degree Received Date" (format "~a/~a" (date-year recv) (date-month recv))))))

@(define external-conf-map (make-multimap))

@(define external-type/c (or/c 'blind 'double-blind 'open))
@(define/contract (conference-external-reviewer
                   #:type type
                   #:conf conf
                   #:number-reviewed n
                   #:start-date start
                   #:end-date end)
   (->i (#:type [type externa-type/c]
         #:conf [conf string?]
         #:number-reviewed [n natural-number/c]
         #:start-date [start date?]
         #:end-date [end date?]
         )
        ()
        #:pre () (set! pre-sidechannel (multimap-length external-conf-map))
        [_ any/c]
        #:post ()
        (= (multimap-length external-conf-map) (add1 pre-sidechannel)))
   (multimap-set!
    external-conf-map
    (date-year start)
    (list
     (cons "Role" "External Reviewer")
     (cons "Review Type" type)
     (cons "Conference" org)
     (cons "Number Reviewed" org)
     (cons "Start Date" (format "~a/~a" (date-year start) (date-month start)))
     (cons "End Date" (format "~a/~a" (date-year start) (date-month start))))))


@(academic-position
  #:title "Assistant Professor"
  #:type 'full-time
  #:start-year 2019
  #:start-month 01
  #:org-name "University of British Columbia"
  #:dept "Computer Science"
  #:school "Faculty of Science"
  #:org-type 'academic
  #:tenure 'tenure-track)

@(academic-position
  #:title "Visiting Scholar"
  #:type 'full-time
  #:start-year 2014
  #:start-month 05
  #:end-year 2014
  #:end-month 07
  #:org-name "Institut Henri Poincarè"
  #:org-type 'academic
  #:tenure 'non-tenure)

@(academic-position
  #:title "Research Intern"
  #:type 'full-time
  #:start-year 2017
  #:start-month 09
  #:end-year 2017
  #:end-month 12
  #:org-name "INRIA"
  #:org-type 'academic
  #:tenure 'non-tenure)

@(academic-position
  #:title "Research/Teaching Assistant"
  #:type 'part-time
  #:start-year 2012
  #:start-month 08
  #:end-year 2018
  #:end-month 10
  #:org-name "Northeastern University"
  #:org-type 'academic
  #:dept "Computer Science"
  #:school "College of Computer and Information Sciences"
  #:tenure 'non-tenure)

@(academic-position
  #:title "Research Assistant"
  #:type 'part-time
  #:start-year 2011
  #:start-month 08
  #:end-year 2012
  #:end-month 05
  #:org-name "Indiana University"
  #:org-type 'academic
  #:dept "Computer Science"
  #:school "School of Informatics and Computing"
  #:tenure 'non-tenure)

@(academic-position
  #:title "Teaching Assistant"
  #:type 'part-time
  #:start-year 2009
  #:start-month 08
  #:end-year 2011
  #:end-month 05
  #:org-name "Indiana University"
  #:org-type 'academic
  #:dept "Computer Science"
  #:school "School of Informatics and Computing"
  #:tenure 'non-tenure)

@;https://www.cicic.ca/928/find_out_if_your_occupation_is_regulated_or_not.canada
@(presentation
  #:title "Do Compilers Respect Programmers?"
  #:event "University of British Columbia"
  #:country CA
  #:city "Vancouver, BC"
  ; TODO make date
  #:year 2018
  #:invited #t)

@(presentation
  #:title "Do Compilers Respect Programmers?"
  #:event "Harvard Computer Science Colloqium"
  #:country US
  #:city "Cambridge, MA"
  ; TODO make date
  #:year 2018
  #:invited #t)

@(presentation
  #:title "Do Compilers Respect Programmers?"
  #:event "Utah Computer Science Colloqium"
  #:country US
  #:city "Cambridge, MA"
  ; TODO make date
  #:year 2018
  #:invited #t)

@(define pldi "ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI)")
@(define icfp "ACM SIGPLAN International Conference on Functional Programming (ICFP)")
@(define popl "ACM SIGPLAN Symposium on Principles of Programming Languages (POPL)")
@(define prisc "ACM SIGPLAN Workshop on Principles of Secure Compilation (PriSC)")
@(define hope "ACM SIGPLAN Workshop on Higher Order Programming with Effects (HOPE)")
@(define rc "Workshop on Reversible Computing")
@(define rc2011 "Proceedings of the 3rd Workshop on Reversible Computing")

@(define pldi18 (format "Proceedings of the 39th ~a" pldi))
@(define pldi15 (format "Proceedings of the 36th ~a" pldi))

@(define icfp17 (format "Proceedings of the 22nd ~a" icfp))
@(define icfp15 (format "Proceedings of the 20th ~a" icfp))

@(define popl18 (format "Proceedings of the ACM on Programming Languages, Volume 2 Issue POPL"))

@(define US "United States")

@(presentation
  #:title "Dependently Typed Aseembly for Secure Compilation"
  #:event prisc
  #:year 2018
  #:country US
  #:city "Los Angeles, CA"
  #:invited #t)

@(presentation
  #:title "Type-Preserving CPS Translation of Σ and Π Types is Not Not Possible"
  #:year 2017
  #:event "INRIA Presecco Seminars"
  #:country "France"
  #:city "Paris"
  #:invited #t)

@(presentation
  #:title "Type-Preserving CPS Translation of Σ and Π Types is Not Not Possible"
  #:year 2017
  #:event "Paris Diderot University"
  #:country "France"
  #:city "Paris"
  #:invited #t)

@(paper
  #:title "Only Control Effects and Dependent Ttpes"
  #:type 'abstract
  #:status 'accepted
  #:year 2017
  #:country "United Kingdom"
  #:city "Oxford"
  #:conference hope
  #:referred #t
  #:invited #f
  #:authors (list "Youyou Cong" "William J. Bowman"))

@(paper
  #:title "Growing a Proof Assistant"
  #:type 'abstract
  #:status 'accepted
  #:year 2016
  #:conference hope
  #:country "Japan"
  #:city "Nara"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman"))

@(paper
  #:title "Typed Closure Conversion of the Calculus of Constructions"
  #:type 'paper
  #:proceedings pldi18
  #:status 'published
  #:year 2018
  #:conference pldi
  #:country US
  #:city "Philidephia, PA"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman" "Amal Ahmed")
  #:page-first 797
  #:page-end 811)

@(paper
  #:title "Type-Preserving CPS Translation of Σ and Π Types is Not Not Possible"
  #:type 'paper
  #:proceedings popl18
  #:status 'published
  #:year 2018
  #:conference popl
  #:country US
  #:city "Los Angeles, CA"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman" "Youyou Cong" "Nick Rioux" "Amal Ahmed"))

@;(journal-paper
@; #:title "Type-Preserving CPS Translation of Σ and Π Types is Not Not Possible"
@; #:type 'paper
@; #:proceedings popl18
@; #:status 'published
@; #:year 2018
@; #:conference popl
@; #:country US
@; #:city "Los Angeles, CA"
@; #:referred #t
@; #:invited #f
@; #:authors (list "William J. Bowman" "Youyou Cong" "Nick Rioux" "Amal Ahmed")
@; #:pages 33)

@(paper
  #:title "Fully Abstract Compilation via Universal Embedding"
  #:type 'paper
  #:proceedings icfp17
  #:status 'published
  #:year 2017
  #:conference icfp
  #:country "Japan"
  #:city "Nara"
  #:referred #t
  #:invited #f
  #:authors (list "Max S. New" "William J. Bowman" "Amal Ahmed")
  #:page-first 103
  #:page-end 116)

@(presentation
  #:title "Towards Type-Preserving Compilation of Coq    ***Student Research Competition Award, First Place***"
  #:year 2017
  #:event (format "~a, Student Research Competition"popl)
  #:country "France"
  #:city "Paris")

@(paper
  #:title "Noninterference for Free"
  #:type 'paper
  #:proceedings icfp15
  #:status 'published
  #:year 2015
  #:conference icfp
  #:country "Canada"
  #:city "Vancouver, BC"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman" "Amal Ahmed")
  #:page-first 101
  #:page-end 113)

@(paper
  #:title "Profile Guided Meta-Programming"
  #:type 'paper
  #:proceedings pldi15
  #:status 'published
  #:year 2015
  #:conference pldi
  #:country US
  #:city "Seattle, Washington"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman" "Swaha Miller" "Vincent St-Amour" "R. Kent Dybvig")
  #:page-first 101
  #:page-end 113)

@(paper
  #:title "Dagger Traced Symmetric Monoidal Categories and Reversible Programming"
  #:type 'paper
  #:proceedings rc2011
  #:status 'published
  #:year 2011
  #:conference rc
  #:country "Belgium"
  #:city "Ghent"
  #:referred #t
  #:invited #f
  #:authors (list "William J. Bowman" "Roshan P. James" "Amr Sabry"))

@(service
  #:role 'committee-member
  #:name "Program Committee"
  #:org "ACM SIGPLAN Workshop on Formal Techniques for Java-like Programs"
  #:start-year 2018
  #:start-month 1
  #:end-year 2018
  #:end-month 7)

@(service
  #:role 'committee-member
  #:name "Program Committee"
  #:org prisc
  #:start-year 2018
  #:start-month 8
  #:end-year 2019
  #:end-month 1)

@(service
  #:role 'chair
  #:name "Student Research Competition Committee"
  #:org icfp
  #:start-year 2018
  #:start-month 10
  #:end-year 2019
  #:end-month 8)

@(service
  #:role 'co-chair
  #:name "General Co-chair"
  #:org "Symposium on Trends in Functional Programming (TFP)"
  #:start-year 2018
  #:start-month 10
  #:end-year 2019
  #:end-month 12)

@(conference-external-reviewer
  #:type 'blind
  #:conf "ACM/IEEE Symposium on Logic in Computer Science (LICS)"
  #:number-reviewed 1
  #:start-date (date 0 0 0 1 2 2018 0 0 #f 0)
  #:end-date (date 0 0 0 1 3 2018 0 0 #f 0))

@(conference-external-reviewer
 #:type 'double-blind
 #:conf popl
 #:number-reviewed 1
 #:start-date (date 0 0 0 31 8 2018 0 0 #f 0)
 #:end-date (date 0 0 0 3 10 2018 0 0 #f 0))

@(recognition
  #:type 'prize/award
  #:name "ACM SIGPLAN POPL Student Research Competition, First Place"
  #:org popl
  #:start-date (date 0 0 0 1 1 2018 0 0 #f 0))

@(recognition
  #:type 'prize/award
  #:name "Northeastern University CCIS Outstanding Service Award"
  #:org "Northeastern University, College of Computer and Information Sciences"
  #:start-date (date 0 0 0 1 3 2018 0 0 #f 0))

@(degree
  #:type "Bachelor's of Science in Computer Science"
  #:org "Indiana University"
  #:start-date (date 0 0 0 1 8 2007  0 0 #f 0)
  #:recv-date (date 0 0 0 1 5 2011 0 0 #f 0)
  #:special "Programming Language")

@(degree
  #:type "Master's of Science in Computer Science"
  #:org "Indiana University"
  #:start-date (date 0 0 0 1 8 2011 0 0 #f 0)
  #:recv-date (date 0 0 0 1 5 2012 0 0 #f 0))

@(degree
  #:type "Doctorate in Computer Science"
  #:org "Northeastern University"
  #:supervisor "Amal Ahmed"
  #:start-date (date 0 0 0 1 8 2012 0 0 #f 0)
  #:recv-date (date 0 0 0 1 12 2018 0 0 #f 0))

@title{William J. Bowman CCV}

@section{Degrees}

@(multimap->itemlist degrees-map #:sort >)

@section{Recognitions}

@(multimap->itemlist recognition-map #:sort >)

@section{Research Specialization Keywords}

Programming languages, meta-programming, compiler correctness, secure
compilation, type-preserving compilation, proof-carrying code, dependent types,
verification

@section{Academic Work Experience}

@(multimap->itemlist position-map #:sort >)

@section{Activities/Assessment and Review Activities}

@subsection{Conference Review Activities}

@(multimap->itemlist external-conf-map #:sort >)

@section{Committee Memberships}

@; TODO: Abstract to enable pulling out committee vs chair
@(multimap->itemlist service-map #:sort >)

@section{Other Memberships}

@itemlist[
          #:style 'ordered
          @tabular[
                   #:sep (hspace 2)
                   #:column-properties '(left left)
                   #:row-properties '(bottom-border top-border)
                   #:cell-properties '((top))
                   (list
                    (list "Membership" "Student Member")
                    (list "Start Date" "2013/2")
                    (list "Organization" "ACM SIGPLAN"))
                   ]
          ]
@; TODO: Add this
@;(multimap->itemlist memberships #:sort >)

@section{Presentations}

@(multimap->itemlist presentations #:sort >)

@section{Conference Papers}

@(multimap->itemlist conf-papers #:sort >)
