(in-package :cl-user)
(defpackage :cl-jenkins
  (:documentation
   "A Common Lisp port of the Python Jenkins library we use at Pure. Use it for
conveniently interacting with Jenkins.")
  (:use :cl :alexandria)
  (:export :jenkins
           :api
           :name
           :auth
           :url
           :host
           :protocol
           :port
           :views
           :view
           :create-view
           :create-or-update-view
           :create-view-with-type
           :jobs
           :job
           :create-job
           :create-or-update-job
           :node-data
           :node
           :nodes
           :quiet-down
           :cancel-quiet-down
           :queue
           :managed-script-ids
           :managed-scripts
           :managed-script
           :create-managed-script
           :create-or-update-managed-script
           :version
           :plugins
           :refresh
           :jenkins-view
           :add-job
           :add-jobs
           :remove-job
           :config
           :exists
           :create-on
           :update
           :destroy
           :jenkins-job
           :color
           :build-with-parameters
           :status
           :enabled
           :disabled
           :enable
           :disable
           :build
           :builds
           :all-builds
           :build-history
           :timer
           :label
           :params
           :timeout
           :submit-description
           :rename
           :config-history
           :param-definitions
           :jenkins-build
           :build-number
           :stop
           :kept-forever
           :toggle-keep-forever
           :keep-forever
           :dont-keep-forever
           :console-lines
           :environment
           :jenkins-node
           :set-online
           :set-offline
           :launch-slave-agent
           :disconnect
           :log-text
           :idle
           :working
           :executors
           :executor
           :current-job
           :job-time-remaining
           :node-labels
           :jenkins-executor
           :ordinal
           :done-in
           :done-in-minutes
           :jenkins-queue
           :items
           :jenkins-queue-item
           :cancel
           :label-filter
           :jenkins-managed-script
           :value
           :description
           :type-tag
           :remparameter
           :id
           :*default-node-label-param-names*))
(in-package :cl-jenkins)

(define-constant +managed-script-id-prefix+
  "org.jenkinsci.plugins.managedscripts.ScriptConfig"
  :test #'string-equal)


(defparameter *default-node-label-param-names*
  (list :node-label :label-name)
  "A list of Jenkins job parameter names to search for a label parameter
as per the Node and Label parameter plugin.")

;; Exported generics

(defgeneric name (thing)
  (:documentation
   "Get the string name of THING. e.g. the canonical name of a
Jenkins, the name of a Jenkins job, etc."))


(defgeneric jenkins (thing)
  (:documentation
   "Get the JENKINS instance to which THING is currently associated"))


(defgeneric url (thing)
  (:documentation
   "Exactly what it says on the tin.  The URL of THING."))


(defgeneric refresh (thing)
  (:documentation
   "Information about THING from the Jenkins server may be cached
locally.  Ensure that this information is brought up to date."))


(defgeneric api (thing)
  (:documentation
   "Give as a plist the Jenkins API info available for THING."))


(defgeneric config (thing)
  (:documentation
   "Get the Jenkins configuration XML string for THING."))


(defgeneric auth (thing)
  (:documentation
   "Get the Jenkins credentials as a pair (user password) required to
make changes to THING."))


(defgeneric exists (thing)
  (:documentation
   "Yields a non-NIL value if THING exists on the Jenkins with which
it is currently associated and NIL if not."))


(defgeneric create-on (thing jenkins)
  (:documentation
   "Create THING on Jenkins JENKINS."))


(defgeneric update (thing config)
  (:documentation
   "Update THING on the Jenkins to which it is currently associated
with the configuration specified by CONFIG."))


(defgeneric destroy (thing)
  (:documentation
   "Delete THING from the Jenkins to which it is currently associated."))


(defgeneric jobs (thing)
  (:documentation
   "Get the list of jobs associated with THING."))


(defgeneric label (thing &optional node-label-param-names)
  (:documentation
   "Get the label expression for THING.  This library supports the
Node and Label parameter plugin, and as such an implementation may
take a list NODE-LABEL-PARAM-NAMES of parameter names to search for
a label expression."))


(defgeneric params (thing)
  (:documentation
   "Get as a plist the build parameters of THING."))


(defgeneric submit-description (thing description)
  (:documentation
   "Set the description of THING to DESCRIPTION on the Jenkins to
which it is currently associated."))

;; Unexported mixins

(defclass jenkins-mixin ()
  ())

(defclass jenkins-creatable-mixin ()
  ())

;; Exported types

(defclass jenkins (jenkins-mixin)
  ((.name :initarg :name :reader name)
   (.auth :initarg :auth :reader auth)
   (.url :reader url)
   (.host :initarg :host :reader host)
   (.protocol :initarg :protocol :reader protocol)
   (.port :initarg :port :reader port)
   .api
   .config
   .version
   .plugins
   .node-data
   .managed-script-ids))


(defclass jenkins-view (jenkins-mixin jenkins-creatable-mixin)
  ((.name :initarg :name :reader name)
   (.jenkins :initarg :jenkins :reader jenkins)
   (.url :initarg :url :reader url)
   .api
   (.config :initarg :config)))


(defclass jenkins-job (jenkins-mixin jenkins-creatable-mixin)
  ((.name :initarg :name :reader name)
   (.color :initarg :color :reader color)
   (.url :initarg :url :reader url)
   (.jenkins :initarg :jenkins :reader jenkins)
   .api
   (.config :initarg :config)
   .params
   .status
   .build-history
   .config-history))


(defclass jenkins-build (jenkins-mixin)
  ((.number :initarg :number :reader build-number)
   (.jenkins :initarg :jenkins :reader jenkins)
   (.url :initarg :url :reader url)
   .api
   .params
   .environment))


(defclass jenkins-node (jenkins-mixin jenkins-creatable-mixin)
  ((.name :initarg :name :reader name)
   (.jenkins :initarg :jenkins :reader jenkins)
   (.url :initarg :url :reader url)
   .api
   .config))


(defclass jenkins-executor (jenkins-mixin)
  ((.url :initarg :url :reader url)
   (.api :initarg :api :reader api)
   .params))


(defclass jenkins-queue (jenkins-mixin)
  ((.jenkins :initarg :jenkins :reader jenkins)
   (.url :reader url)
   .api))


(defclass jenkins-queue-item (jenkins-mixin)
  ((.jenkins :initarg :jenkins :reader jenkins)
   (.id :initarg :id :reader id)
   (.api :initarg :api :reader api)
   (.url :reader url)
   .params))


(defclass jenkins-managed-script (jenkins-mixin jenkins-creatable-mixin)
  ((.jenkins :initarg :jenkins :reader jenkins)
   (.id :initarg :id :reader id)
   (.config :initarg :config)
   .api))


(defclass build-parameter-definitions ()
  ((.config :initarg :config)))


;; Unexported basic utilities and helper generics

(defun jenkins-api-normalizer (key)
  (let ((str (apply #'concatenate
                    'string
                    (map 'list
                         #'(lambda (ch)
                             (cond ((upper-case-p ch) (format nil "-~A" ch))
                                   ((char= ch #\_) "-")
                                   (t (string (char-upcase ch)))))
                         key))))
    (if (char= #\- (char str 0))
        (subseq str 1)
        str)))


(defgeneric api-updater (thing new-api)
  (:documentation
   "Since some functions internally update the local API, we define a
 setf expander for api. We make it generic so it can be implemented
 differently for different classes, to be consistent with api being
 generic.")
  (:method ((thing jenkins-mixin) new-api)
    (setf (slot-value thing '.api) new-api)))

(defsetf api api-updater)


(defgeneric default-api-endpoint (thing)
  (:method ((thing jenkins-mixin))
    "api/json"))


(defgeneric api-args (thing)
  (:method ((thing jenkins-mixin))
    ()))


(defun api-call (thing &key url target auth
                       content (content-type "application/octet-stream")
                       (timeout 60) args method) ;(verbose t))
  (drakma:http-request (format nil "~A/~A~A"
                               (or url (string-right-trim '(#\/) (url thing)))
                               (or target (default-api-endpoint thing))
                               (if args (format nil "?~A" args) ""))
                       :content content
                       :method (or method (if content :post :get))
                       :content-type content-type
                       :basic-authorization auth
                       :connection-timeout timeout
                       :preserve-uri t))


(defgeneric primary-key (thing)
  (:method ((thing jenkins-mixin))
    #'name))


(defgeneric process-api-call (thing api-response)
  (:method ((thing jenkins-mixin) api-response)
    (jonathan.decode:parse (copy-seq api-response)
                           :as :plist
                           :keyword-normalizer #'jenkins-api-normalizer
                           :normalize-all t)))


(defgeneric config-api-target (thing)
  (:method ((thing jenkins-mixin))
    "config.xml"))


(defgeneric api-key (thing))


(defgeneric create-command (thing))


(defun default-creatable-url (creatable)
  (format nil "~A/~A/~A"
          (url (jenkins creatable))
          (api-key creatable)
          (drakma:url-encode (funcall (primary-key creatable) creatable)
                             :utf-8)))


(defun encode-params (params &key (key-normalizer #'string-downcase))
  (format nil "~{~A=~A~^&~}"
          (mapcar #'(lambda (x)
                      (drakma:url-encode (if (keywordp x)
                                             (funcall key-normalizer (symbol-name x))
                                             x)
                                         :utf-8))
                  params)))


(defun basic-refresh (thing)
  (slot-makunbound thing '.api)
  (slot-makunbound thing '.config))


(defun encode-jenkins-tree (tree)
  (format nil "tree=~A" (drakma:url-encode tree :utf-8)))


(defun timestamp-from-jenkins (jenkins-time)
  (+ (round jenkins-time 1000) (encode-universal-time 0 0 0 1 1 1970 0)))


(defun normalize-key (key)
  "Use to Lispify JSON keys.  Upcases all letters in the key, and
converts underscores to dashes so that they can be interned as
symbols."
  (map 'string #'(lambda (x) (if (char= x #\_) #\- (char-upcase x))) key))


(defun make-param-pair (param &key (key #'identity))
  (list (intern (normalize-key (getf (funcall key param) :name))
                :keyword)
        (getf (funcall key param) :value)))


;; Default implementations of exported generics

(defmethod refresh ((thing jenkins-mixin))
  (basic-refresh thing))


(defmethod api ((thing jenkins-mixin))
  (unless (slot-boundp thing '.api)
    (setf (slot-value thing '.api)
          (process-api-call thing
                            (api-call thing :args (api-args thing)))))
  (slot-value thing '.api))


(defmethod config ((thing jenkins-mixin))
  (unless (slot-boundp thing '.config)
    (setf (slot-value thing '.config)
          (api-call thing
                    :target (config-api-target thing)
                    :auth (auth thing))))
  (slot-value thing '.config))


(defmethod auth ((thing jenkins-mixin))
  (auth (jenkins thing)))


(defmethod exists ((thing jenkins-mixin))
  (api thing))


(defmethod create-on ((thing jenkins-creatable-mixin) (jenkins jenkins))
  (api-call thing
            :url (url jenkins)
            :target (create-command thing)
            :content (config thing)
            :content-type "text/xml"
            :auth (auth jenkins))
  (refresh thing)
  (refresh jenkins)
  (funcall (symbol-function (intern (string-upcase (api-key thing)) :cl-jenkins))
           jenkins
           (name thing)))


(defmethod update ((thing jenkins-creatable-mixin) (config string))
  (api-call thing
            :target "config.xml"
            :content config
            :content-type "text/xml"
            :auth (auth thing))
  (refresh thing)
  thing)


(defmethod destroy ((thing jenkins-creatable-mixin))
  (api-call thing :target "doDelete" :auth (auth thing) :method :post)
  (refresh thing))


;; Jenkins specific things

(defun views (jenkins)
  "Get a list of JENKINS-VIEW instances corresponding to the views
defined on JENKINS."
  (mapcar #'(lambda (v)
              (apply #'make-instance 'jenkins-view :jenkins jenkins v))
          (getf (api jenkins) :views)))


(defun view (jenkins view-name)
  "Get a JENKINS-JOB instance for the job named JOB-NAME on JENKINS if such a
job exists, NIL if not."
  (let ((view-info (car (member view-name
                                (getf (api jenkins) :views)
                                :key #'(lambda (x) (getf x :name))
                                :test #'string-equal))))
    (if view-info
        (apply #'make-instance 'jenkins-view :jenkins jenkins view-info))))


(defun create-view (jenkins view-name config)
  "Create view named VIEW-NAME with config CONFIG on JENKINS."
  (create-on (make-instance 'jenkins-view
                            :jenkins jenkins
                            :name view-name
                            :config config)
             jenkins))


(defun create-or-update-view (jenkins view-name config)
  "Create view named VIEW-NAME with config CONFIG on JENKINS, or if a
view named VIEW-NAME already exists, update its configuration with
CONFIG."
  (let ((view (view jenkins view-name)))
    (if view
        (update view config)
        (create-view jenkins view-name config))))


(defun create-view-with-type (jenkins view-name
                              &optional (view-type "hudson.model.ListView"))
  (let ((view-args (list :|name| (drakma:url-encode view-name :utf-8)
                         :|mode| view-type)))
    (setf (getf view-args :json) (jonathan.encode:to-json view-args))
    (api-call jenkins
              :url (url jenkins)
              :target (format nil "createView?~A" (encode-params view-args))
              :auth (auth jenkins)
              :method :post)
    (refresh jenkins)
    (view jenkins view-name)))


(defmethod jobs ((jenkins jenkins))
  "Get a list of JENKINS-JOB instances corresponding to the jobs
defined on JENKINS."
  (mapcar #'(lambda (j)
              (apply #'make-instance 'jenkins-job :jenkins jenkins :allow-other-keys t j))
          (getf (api jenkins) :jobs)))


(defun job (jenkins job-name)
  "Get a JENKINS-JOB instance for the job named JOB-NAME on JENKINS if such a
job exists, NIL if not."
  (let ((job-info (car (member job-name
                               (getf (api jenkins) :jobs)
                               :key #'(lambda (x) (getf x :name))
                               :test #'string-equal))))
    (if job-info
        (apply #'make-instance 'jenkins-job :jenkins jenkins :allow-other-keys t job-info))))


(defun create-job (jenkins job-name config)
  "Create job named JOB-NAME with config CONFIG on JENKINS."
  (create-on (make-instance 'jenkins-job
                            :jenkins jenkins
                            :name job-name
                            :config config)
             jenkins))


(defun create-or-update-job (jenkins job-name config)
  "Create job named JOB-NAME with config CONFIG on JENKINS, or if a
job named JOB-NAME already exists, update its configuration with
CONFIG."
  (let ((job (job jenkins job-name)))
    (if job
        (update job config)
        (create-job jenkins job-name config))))


(defun node-data (jenkins)
  (unless (slot-boundp jenkins '.node-data)
    (setf (slot-value jenkins '.node-data)
          (process-api-call jenkins
                            (api-call jenkins
                                      :target (format nil "computer/~A"
                                                      (default-api-endpoint jenkins)))))
    (setf (slot-value jenkins '.node-data)
          (getf (slot-value jenkins '.node-data) :computer)))
  (slot-value jenkins '.node-data))


(defun node (jenkins node-name)
  (let ((node-info (car (member node-name
                               (node-data jenkins)
                               :key #'(lambda (x) (getf x :display-name))
                               :test #'string-equal))))
    (if node-info
        (make-instance 'jenkins-node
                       :jenkins jenkins
                       :name (getf node-info :display-name)
                       :url (format nil "~A/computer/~A"
                                    (url jenkins)
                                    (getf node-info :display-name))))))


(defun nodes (jenkins)
  "Get a list of JENKINS-NODE instances corresponding to the testbeds
defined on JENKINS."
  (loop for node in (node-data jenkins)
        if (not (or (search "executioner" (getf node :display-name))
                    (string-equal (getf node :display-name) "master")))
          collect (make-instance 'jenkins-node
                       :jenkins jenkins
                       :name (getf node :display-name)
                       :url (format nil "~A/computer/~A"
                                    (url jenkins)
                                    (getf node :display-name)))))


(defun quiet-down (jenkins)
  (api-call jenkins :target "quietDown" :auth (auth jenkins) :method :post))


(defun cancel-quiet-down (jenkins)
  (api-call jenkins :target "cancelQuietDown" :auth (auth jenkins) :method :post))


(defun queue (jenkins)
  (make-instance 'jenkins-queue :jenkins jenkins))


(defun managed-script-ids (jenkins)
  (unless (slot-boundp jenkins '.managed-script-ids)
    (flet ((href (anchor)
             (or (cadr (assoc "href" (cadr anchor) :test #'string-equal)) "")))
      (let ((soup (xmls:parse-to-list (api-call jenkins
                                                :target "configfiles"
                                                :auth (auth jenkins)))))
        (setf (slot-value jenkins '.managed-script-ids)
              (loop for anchor in (xml-utils:find-all soup "a")
                    if (search +managed-script-id-prefix+ (href anchor))
                      collect (car
                               (last
                                (cl-utilities:split-sequence #\= (href anchor))))))
        (setf (slot-value jenkins '.managed-script-ids)
              (delete-duplicates (slot-value jenkins '.managed-script-ids)
                                 :test #'string-equal)))))
  (slot-value jenkins '.managed-script-ids))


(defun managed-scripts (jenkins)
  (mapcar #'(lambda (id)
              (make-instance 'jenkins-managed-script :jenkins jenkins :id id))
          (managed-script-ids jenkins)))


(defun managed-script (jenkins script-id)
  (if (member script-id (managed-script-ids jenkins) :test #'string-equal)
      (make-instance 'jenkins-managed-script :jenkins jenkins :id script-id)))


(defun create-managed-script (jenkins script-id config)
  "Create managed script with id SCRIPT-ID and config CONFIG on JENKINS."
  (create-on (make-instance 'jenkins-managed-script
                            :jenkins jenkins
                            :id script-id
                            :config config)
             jenkins))


(defun create-or-update-managed-script (jenkins script-id config)
  "Create managed script with id SCRIPT-ID and config CONFIG on
JENKINS, or if a script with SCRIPT-ID already exists, update its
configuration with CONFIG."
  (let ((script (managed-script jenkins script-id)))
    (if script
        (update script config)
        (create-managed-script jenkins script-id config))))


(defun version (jenkins)
  "Get the version of the server running on JENKINS."
  (unless (slot-boundp jenkins '.version)
      (setf (slot-value jenkins '.version)
            (multiple-value-bind (body status headers)
                (drakma:http-request (url jenkins))
              (declare (ignore body status))
              (cdr (assoc :x-jenkins headers)))))
  (slot-value jenkins '.version))


(defun plugins (jenkins)
  "Get a plist of information on all the plugins installed on JENKINS."
  (unless (slot-boundp jenkins '.plugins)
      (setf (slot-value jenkins '.plugins)
            (process-api-call jenkins
                              (api-call jenkins
                                        :url (format nil "~A/pluginManager"
                                                     (url jenkins))
                                        :args "depth=1"))))
  (slot-value jenkins '.plugins))


(defmethod shared-initialize :after ((jenkins jenkins) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (when (and (not (slot-boundp jenkins '.host))
             (slot-boundp jenkins '.name))
    (setf (slot-value jenkins '.host)
          (name jenkins)))
  (when (and (not (slot-boundp jenkins '.url))
             (every (curry #'slot-boundp jenkins)
                    '(.protocol .host .port)))
    (setf (slot-value jenkins '.url)
          (format nil "~A://~A:~A"
                  (protocol jenkins)
                  (host jenkins)
                  (port jenkins)))))


(defmethod refresh ((jenkins jenkins))
  (basic-refresh jenkins)
  (slot-makunbound jenkins '.plugins)
  (slot-makunbound jenkins '.version)
  (slot-makunbound jenkins '.node-data)
  (slot-makunbound jenkins '.managed-script-ids))


(defmethod config-api-target ((jenkins jenkins))
  "computer/%28master%29/config.xml")


;; View specific things

(defmethod jobs ((view jenkins-view))
  (mapcar #'(lambda (j)
              (apply #'make-instance 'jenkins-job :jenkins (jenkins view) j))
          (getf (api view) :jobs)))


(defun add-job (view job)
  (api-call view
            :target (format nil "addJobToView?name=~A"
                            (drakma:url-encode (name job) :utf-8))
            :auth (auth view)
            :method :post))


(defun add-jobs (view jobs)
  (mapc #'(lambda (j) (add-job view j)) jobs))


(defun remove-job (view job)
  (api-call view
            :target (format nil "removeJobFromView?name=~A"
                            (drakma:url-encode (name job) :utf-8))
            :auth (auth view)
            :method :post))


(defmethod api-key ((view jenkins-view))
  "view")


(defmethod create-command ((view jenkins-view))
  (format nil "createView?name=~A" (drakma:url-encode (name view) :utf-8)))


(defmethod destroy ((view jenkins-view))
  (unless (string-equal (name view) "all")
    (call-next-method view)))


;; Job specific things

(defun build-with-parameters (job &rest params)
  "Start a build of JOB with parameters specified by PARAMS.  PARAMS
should be a plist of keyword value pairs.  Any \#- characters in the
keywords will be converted to #\_, such that :THIS-THING will become
THIS_THING before being passed to Jenkins."
  (api-call job
            :target (format nil "buildWithParameters?~A"
                            (encode-params params
                                           :key-normalizer #'(lambda (x)
                                                               (substitute #\_ #\- x))))
            :auth (auth job)
            :method :post))


(defun status (job)
  "Job status is represented in the jenkins API by the \"color\"
attribute of jobs, with success bing \"blue\" and fail being
\"red\". In either of those cases, I want those to be translated to
their human-readable equivalents. If the color is something else,
then just return that instead."
  (unless (slot-boundp job '.status)
    (setf (slot-value job '.status)
          (cond ((search "blue" (color job)) "pass")
                ((search "red" (color job)) "fail")
                (t (color job)))))
  (slot-value job '.status))


(defun enabled (job)
  (getf (api job) :buildable))


(defun disabled (job)
  (not (getf (api job) :buildable)))


(defun enable (job)
  (api-call job :target "enable" :auth (auth job) :method :post))


(defun disable (job)
  (api-call job :target "disable" :auth (auth job) :method :post))


(defun build (job build-number)
  (let ((url (format nil "~A/~A" (url job) build-number)))
    (if (member build-number
                (getf (api job) :builds)
                :key #'(lambda (x) (getf x :number))
                :test #'=)
        (make-instance 'jenkins-build
                       :jenkins (jenkins job)
                       :number build-number
                       :url url)
        (multiple-value-bind (body status headers) (api-call job :url url)
          (declare (ignore headers))
          (when (= status 200)
            (push (process-api-call job body) (getf (api job) :builds))
            (make-instance 'jenkins-build
                           :jenkins (jenkins job)
                           :number build-number
                           :url url))))))


(defun make-build-from-api (job build)
  (make-instance 'jenkins-build
                 :jenkins (jenkins job)
                 :number (getf build :number)
                 :url (format nil "~A/~A"
                              (url job)
                              (getf build :number))))


(defun builds (job)
  (mapcar #'(lambda (build)
              (make-build-from-api job build))
          (getf (api job) :builds)))


(defun all-builds (job)
  (let ((tree (encode-jenkins-tree "allBuilds,allBuilds[url,number,id,result,timestamp,description,building,builtOn]")))
    (mapcar #'(lambda (build)
                (make-build-from-api job build))
            (getf (process-api-call job (api-call job :args tree))
                  :all-builds))))


(defun build-history (job &key (days 7) (ignore-building t))
  (unless (slot-boundp job '.build-history)
    (setf (slot-value job '.build-history) ()))
  (if (assoc days (slot-value job '.build-history))
      (cdr (assoc days (slot-value job '.build-history)))
      (let ((recent-builds (loop for build in (builds job)
                                 ; builds are ordered, so once we
                                 ; overstep the days we stop.
                                 until (> (- (get-universal-time)
                                              (timestamp-from-jenkins
                                               (getf (api build) :timestamp)))
                                          (* 24 60 60 days))
                                 if (or (not ignore-building)
                                        (not (getf (api build) :building)))
                                   collect build)))
        (push (cons days recent-builds) (slot-value job '.build-history))
        recent-builds)))


(defun timer (job)
  (caddr (xml-utils:find-first (xml-utils:find-first (xmls:parse-to-list (config job))
                                                     "hudson.triggers.TimerTrigger")
                             "spec")))


(defun set-timer (job new-timer)
  (let* ((soup (xmls:parse-to-list (config job)))
         (timer-node (xml-utils:find-first
                      (xml-utils:find-first soup "hudson.triggers.TimerTrigger")
                      "spec")))
    (when timer-node
          (setf (caddr timer-node) new-timer)
          (setf (slot-value job '.config) (xmls:toxml (xmls:nodelist->node soup))))
    new-timer))


(defsetf timer set-timer)


(defmethod label ((job jenkins-job) &optional node-label-param-names)
  (declare (ignore node-label-param-names))
  (let* ((soup (xmls:parse-to-list (config job)))
         (label-node (xml-utils:find-first soup
                                           "org.jvnet.jenkins.plugins.nodelabelparameter.LabelParameterDefinition")))
    (if label-node
        (caddr (xml-utils:find-first label-node "defaultValue"))
        (caddr (xml-utils:find-first soup "assignedNode")))))


(defgeneric set-label (job new-label)
  (:method ((job jenkins-job) (new-label string))
    (let* ((soup (xmls:parse-to-list (config job)))
           (label-node (xml-utils:find-first soup
                                             "org.jvnet.jenkins.plugins.nodelabelparameter.LabelParameterDefinition")))
      (when label-node
        (setf (caddr (xml-utils:find-first label-node "defaultValue"))
              new-label)
        (setf (slot-value job '.config) (xmls:toxml (xmls:nodelist->node soup))))
      new-label)))


(defsetf label set-label)


(defmethod params ((job jenkins-job))
  (unless (slot-boundp job '.params)
    (setf (slot-value job '.params)
          (mapcan #'(lambda (item)
                      (mapcan #'(lambda (param)
                                  (make-param-pair
                                   param
                                   :key #'(lambda (x)
                                            (getf x :default-parameter-value))))
                              (getf item :parameter-definitions)))
                  (getf (api job) :actions))))
  (slot-value job '.params))


(defun timeout (job)
  (let* ((soup (xmls:parse-to-list (config job)))
         (timeout-node (xml-utils:find-first soup
                                             "hudson.plugins.build__timeout.BuildTimeoutWrapper"))
         (timeout-val (or (xml-utils:find-first timeout-node "timeoutMinutes")
                          (xml-utils:find-first timeout-node "timeoutSecondsString"))))
    (caddr timeout-val)))


(defmethod submit-description ((job jenkins-job) (description string))
  (api-call job
            :target "submitDescription"
            :content (format nil "description=~A"
                             (drakma:url-encode description :utf-8))
            :content-type "application/x-www-form-urlencoded"
            :auth (auth job)))


(defun rename (job new-name)
  (multiple-value-bind (body status headers reply-url stream close-stream reason)
      (api-call job
                :target "doRename"
                :content (format nil "newName=~A"
                                 (drakma:url-encode new-name :utf-8))
                :content-type "application/x-www-form-urlencoded"
                :auth (auth job))
    (when (= status 200)
      (setf (slot-value job '.name) new-name)
      (setf (slot-value job '.url)
            (format nil "~A/job/~A" (url (jenkins job)) new-name))
      (refresh job)
      (refresh (jenkins job)))
    (values body status headers reply-url stream close-stream reason)))


(defun config-history (job)
  (unless (slot-boundp job '.config-history)
    (setf (slot-value job '.config-history)
          (mapc #'(lambda (x)
                    (setf (getf x :date)
                          (destructuring-bind (year month day hour minute second)
                              (mapcan #'(lambda (y)
                                          (mapcar #'parse-integer
                                                  (cl-utilities:split-sequence #\- y)))
                                      (cl-utilities:split-sequence #\_ (getf x :date)))
                            (encode-universal-time second minute hour day month year))))
                (getf (process-api-call job
                                        (api-call job
                                                  :target "jobConfigHistory/api/json"
                                                  :auth (auth job)))
                      :job-config-history))))
  (slot-value job '.config-history))


(defun param-definitions (job)
  (make-instance 'build-parameter-definitions :config (config job)))


(defmethod refresh ((job jenkins-job))
  (basic-refresh job)
  (slot-makunbound job '.status)
  (slot-makunbound job '.config-history)
  (slot-makunbound job '.build-history)
  (slot-makunbound job '.params))


(defmethod api-key ((job jenkins-job))
  "job")


(defmethod api-args ((job jenkins-job))
  (encode-jenkins-tree "*,actions[*,parameterDefinitions[*,defaultParameterValue[*]]],builds[url,number,id,result,timestamp,description,building,builtOn]"))


(defmethod create-command ((job jenkins-job))
  (format nil "createItem?name=~A" (drakma:url-encode (name job) :utf-8)))


;; Build specific things

(defmethod submit-description ((build jenkins-build) (description string))
  (api-call build
            :target "submitDescription"
            :content (format nil "description=~A&Submit=Submit" description)
            :content-type "application/x-www-form-urlencoded"
            :auth (auth build)))


(defmethod params ((build jenkins-build))
  (unless (slot-boundp build '.params)
    (setf (slot-value build '.params)
          (mapcan #'(lambda (item)
                      (mapcan #'make-param-pair
                              (getf item :parameters)))
                  (getf (api build) :actions))))
  (slot-value build '.params))


(defun stop (build)
  (api-call build :target "stop" :auth (auth build) :method :post))


(defun kept-forever (build)
  (getf (api build) :keep-log))


(defun toggle-keep-forever (build)
  (api-call build :target "toggleLogKeep" :auth (auth build)))


(defun keep-forever (build)
  (unless (kept-forever build) (toggle-keep-forever build)))


(defun dont-keep-forever (build)
  (when (kept-forever build) (toggle-keep-forever build)))


(defun console-lines (build)
  (multiple-value-bind (body status headers)
      (drakma:http-request (format nil "~A/consoleText" (url build)))
    (declare (ignore headers))
    (if (= status 200)
        (cl-utilities:split-sequence #\Newline body))))


(defun environment (build)
  (unless (slot-boundp build '.environment)
    (setf (slot-value build '.environment)
          (getf (jonathan.decode:parse (copy-seq
                                        (api-call build
                                                  :target "injectedEnvVars/api/json"))
                                       :as :plist
                                       :keyword-normalizer #'normalize-key
                                       :normalize-all t)
                :envmap)))
  (slot-value build '.environment))


(defmethod refresh ((build jenkins-build))
  (basic-refresh build)
  (slot-makunbound build '.params)
  (slot-makunbound build '.environment))


(defmethod api-args ((build jenkins-build))
  (encode-jenkins-tree "*,artifacts[*],actions[*,parameters[*],causes[*]]"))


(defmethod primary-key ((build jenkins-build))
  #'build-number)


;; Node specific things

(defun set-online (node)
  (if (getf (api node) :temporarily-offline)
      (api-call node
                :target "toggleOffline"
                :auth (auth node)
                :method :post)))


(defun set-offline (node)
  (if (not (getf (api node) :temporarily-offline))
      (api-call node
                :target "toggleOffline"
                :auth (auth node)
                :method :post)))


(defun launch-slave-agent (node)
  (api-call node
            :target "launchSlaveAgent"
            :auth (auth node)
            :method :post))


(defun disconnect (node)
  (api-call node
            :target "doDisconnect"
            :auth (auth node)
            :method :post))


(defun log-text (node)
  (api-call node :target "logText/progressiveHtml"))


(defun idle (node)
  (getf (api node) :idle))


(defun working (node)
  (not (idle node)))


(defun executors (node)
  (mapcar #'(lambda (e)
              (make-instance 'jenkins-executor :api e :url (url node)))
          (getf (api node) :executors)))


(declaim (ftype (function (jenkins-executor) number) ordinal))
(defun executor (node number)
  (car (member number (executors node) :key #'ordinal)))


(defun current-job (node)
  (executor node 0))


(declaim (ftype (function (jenkins-executor) number) done-in))
(defun job-time-remaining (node)
  (done-in (current-job node)))


(defun node-labels (node)
  (let ((labels-node (xml-utils:find-first (xmls:parse-to-list (config node)) "label")))
    (if labels-node
        (cl-utilities:split-sequence #\Space (caddr labels-node)))))


(defmethod api-args ((node jenkins-node))
  "depth=2")


(defmethod api-key ((node jenkins-node))
  "computer")


(defmethod create-command ((node jenkins-node))
  (format nil "createItem?name=~A" (drakma:url-encode (name node) :utf-8)))


;; Executor specific stuff

(defun ordinal (executor)
  (getf (api executor) :number))


(defmethod name (executor)
  (getf (api executor) :full-display-name))


(defmethod params ((executor jenkins-executor))
  (unless (slot-boundp executor '.params)
    (setf (slot-value executor '.params)
          (mapcan #'(lambda (action)
                      (mapcan #'make-param-pair
                              (getf action :parameters)))
                  (getf (api executor) :actions))))
  (slot-value executor '.params))


(defun done-in (executor)
  (if (getf (api executor) :estimated-duration)
      (round (* (/ (getf (api executor) :estimated-duration) 1000)
                (- 1 (* (getf (api executor) :progress) 0.01))))
      0))


(defun done-in-minutes (executor)
  (round (done-in executor) 60))


(defmethod shared-initialize ((executor jenkins-executor) slot-names &rest initargs)
  (apply #'call-next-method executor slot-names initargs)
  (setf (slot-value executor '.api) (copy-tree (slot-value executor '.api)))
  (loop for key in (getf (api executor) :current-executable) by #'cddr do
    (unless (member key (api executor))
      (setf (getf (api executor) key)
            (getf (getf (api executor) :current-executable) key)))))


(defmethod primary-key ((executor jenkins-executor))
  #'ordinal)


;; Queue speicific stuff

(defun items (queue)
  (mapcar #'(lambda (item)
              (make-instance 'jenkins-queue-item
                             :jenkins (jenkins queue)
                             :id (getf item :id)
                             :api item))
          (getf (api queue) :items)))


(defmethod shared-initialize ((queue jenkins-queue) slot-names &rest initargs)
  (apply #'call-next-method queue slot-names initargs)
  (setf (slot-value queue '.url) (format nil "~A/~A"
                                         (url (jenkins queue))
                                         (api-key queue))))


(defmethod primary-key ((queue jenkins-queue))
  #'jenkins)


(defmethod api-key ((queue jenkins-queue))
  "queue")


(defmethod api-args ((queue jenkins-queue))
  (encode-jenkins-tree "items[*,params,task[description,displayName,name,url,buildable,color],actions[*,causes[*],parameters[*]]]"))


;; Queue Item speicific stuff

(defun cancel (item)
  (api-call item
            :url (url (queue (jenkins item)))
            :target "cancelItem"
            :args (format nil "id=~A"
                          (drakma:url-encode (princ-to-string (id item))
                                             :utf-8))
            :auth (auth (jenkins item))
            :method :post))


(defmethod label ((item jenkins-queue-item) &optional (node-label-param-names *default-node-label-param-names*))
  (labels ((get-string-param (key)
             (loop for param in (cl-utilities:split-sequence #\Newline
                                                             (getf (api item) :params))
                   if (search key param)
                     collect (remove #\Space
                                     (string-trim '(#\' #\[ #\])
                                                  (cadr (cl-utilities:split-sequence #\= param))))))
           (get-match (pattern string)
             (let ((matches (multiple-value-list (cl-ppcre:scan pattern string))))
               (if (car matches)
                   (subseq string
                           (aref (caddr matches) 0)
                           (aref (cadddr matches) 0))))))
    (let ((label (or (some #'(lambda (p)
                               (getf (params item) p))
                           node-label-param-names)
                     (some #'(lambda (p)
                               (car (get-string-param (substitute #\_ #\- (symbol-name p)))))
                           node-label-param-names)
                     (get-match "Waiting for next available executor on( .*)"
                                (getf (api item) :why ""))
                     (get-match "All nodes of label( .*)are offline"
                                (getf (api item) :why ""))
                     (string-trim '(#\Space #\Tab #\Newline)
                                  (get-match "(.*)is offline"
                                             (getf (api item) :why "")))
                     (string-trim '(#\Space #\Tab #\Newline)
                                  (get-match "There are no nodes with the label (.*)"
                                             (getf (api item) :why ""))))))
      (if label (remove #\" label)))))


(defun label-filter (item)
  (let ((label (label item)))
    (if label
        (label-matcher:compile-expression label))))


(defmethod params ((item jenkins-queue-item))
  (unless (slot-boundp item '.params)
    (setf (slot-value item '.params)
          (mapcan #'(lambda (action)
                      (mapcan #'make-param-pair
                              (getf action :parameters)))
                  (getf (api item) :actions))))
  (slot-value item '.params))


(defmethod shared-initialize ((item jenkins-queue-item) slot-names &rest initargs)
  (apply #'call-next-method item slot-names initargs)
  (setf (slot-value item '.api) (copy-tree (slot-value item '.api)))
  (setf (slot-value item '.url)
        (format nil "~A/~A/~A"
                (url (jenkins item))
                (api-key item)
                (drakma:url-encode (princ-to-string (id item))
                                   :utf-8))))


(defmethod primary-key ((item jenkins-queue-item))
  #'id)


(defmethod api-key ((item jenkins-queue-item))
  "queue/item")


;; Managed Script specific stuff

(defmethod url ((script jenkins-managed-script))
  (url (jenkins script)))


(defmethod process-api-call ((script jenkins-managed-script) api-response)
  (flet ((named (input name)
           (equal (assoc "name" (cadr input) :test #'string-equal)
                  (list "name" name)))
         (value (input)
           (cadr (assoc "value" (cadr input) :test #'string-equal))))
    (let ((soup (xmls:parse-to-list api-response)) api)
      (loop for input in (xml-utils:find-all soup "input") do
        (loop for key in (list :id :name :comment) do
          (when (named input (format nil "config.~(~A~)" (symbol-name key)))
            (setf (getf api key) (value input))))
        (if (and (named input "name") (value input))
            (push (value input) (getf api :args))))
      (setf (getf api :content)
            (caddr (xml-utils:find-first soup "textarea")))
      api)))


(defmethod primary-key ((script jenkins-managed-script))
  #'id)


(defmethod api-key ((script jenkins-managed-script))
  "configfiles/editConfig")


(defmethod default-api-endpoint ((script jenkins-managed-script))
  (api-key script))


(defmethod api-args ((script jenkins-managed-script))
  (format nil "id=~A" (id script)))


(defmethod create-command ((script jenkins-managed-script))
  "configfiles/saveConfig")


(defmethod create-on ((script jenkins-managed-script) (jenkins jenkins))
  (api-call script
            :url (url jenkins)
            :target (create-command script)
            :content (config script)
            :content-type "application/x-www-form-urlencoded"
            :auth (auth jenkins))
  (refresh jenkins)
  (managed-script jenkins (id script)))


(defmethod update ((script jenkins-managed-script) (config string))
  (api-call script
            :target (create-command script)
            :content config
            :content-type "application/x-www-form-urlencoded"
            :auth (auth script))
  (refresh script)
  script)


(defmethod destroy ((script jenkins-managed-script))
  (api-call script
            :target "configfiles/removeConfig"
            :args (api-args script)
            :auth (auth script)
            :method :post)
  (refresh script))


(defmethod config ((script jenkins-managed-script))
  (unless (slot-boundp script '.config)
    (setf (slot-value script '.config)
          (let* ((query-params (list :id :name :comment :content))
                 (script-json (mapcan #'(lambda (key)
                                          (list (intern (string-downcase (symbol-name key))
                                                        :keyword)
                                                (getf (api script) key "")))
                                      query-params)))
            (setf (getf script-json :|args|)
                  (mapcar #'(lambda (arg)
                              (list :|name| arg))
                          (getf (api script) :args)))
            (setf (getf script-json :|stapler-class|) +managed-script-id-prefix+)
            (encode-params (nconc (list "stapler-class" +managed-script-id-prefix+
                                        "Submit" "Submit")
                                  (mapcan #'(lambda (key)
                                              (list (format nil "config.~(~A~)" key)
                                                    (getf (api script) key "")))
                                          query-params)
                                  (mapcan #'(lambda (arg)
                                              (list "name" arg))
                                          (getf (api script) :args))
                                  (list "json"
                                        (jonathan.encode:to-json (list :|config|
                                                                       script-json))))))))
  (slot-value script '.config))


(defmethod api ((script jenkins-managed-script))
  (unless (slot-boundp script '.api)
    (setf (slot-value script '.api)
          (process-api-call script
                            (api-call script
                                      :args (api-args script)
                                      :auth (auth script)))))
  (slot-value script '.api))


;; Build Parameter Definitions specific stuff

(defmethod shared-initialize ((params build-parameter-definitions) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (slot-value params '.config)
        (xmls:parse-to-list (getf initargs :config))))


(defmethod config ((params build-parameter-definitions))
  (xmls:toxml (xmls:nodelist->node (slot-value params '.config))))


(defun parameter-definitions-tree (params)
  (cddr (xml-utils:find-first (slot-value params '.config) "parameterDefinitions")))


(defun param-name (param)
  (intern (normalize-key (caddr (assoc "name"
                                       (cddr param)
                                       :test #'string-equal)))
          :keyword))


(defun param-value-list (param)
  (assoc "defaultValue" (cddr param) :test #'string-equal))


(defun param-description-list (param)
  (assoc "description" (cddr param) :test #'string-equal))


(defmethod params ((params build-parameter-definitions))
  (mapcan #'(lambda (param)
              (list (param-name param) (caddr (param-value-list param))))
          (parameter-definitions-tree params)))


(defun contains-param (params indicator)
  (member indicator
          (parameter-definitions-tree params)
          :key #'param-name))


(defun get-param (params indicator)
  (car (contains-param params indicator)))


(defun param-tag-string (tag)
  (format nil "hudson.model.~:(~A~)ParameterDefinition" tag))

(defun new-param-node (indicator &key value description (tag :string))
  (list (param-tag-string tag) nil
        (list "name"
              nil
              (substitute #\_ #\- (symbol-name indicator)))
        (list* "description" nil (if description (list description)))
        (list* "defaultValue" nil (if value (list value)))))


(defun value (params indicator)
  (caddr (param-value-list (get-param params indicator))))


(defun set-value (params indicator value)
  (let ((param (get-param params indicator)))
    (if param
        (setf (caddr (param-value-list param)) value)
        (setf (cdr (last (parameter-definitions-tree params)))
              (list (new-param-node indicator :value value))))
    value))


(defsetf value set-value)


(defun description (params indicator)
  (caddr (param-description-list (get-param params indicator))))


(defun set-description (params indicator value)
  (let ((param (get-param params indicator)))
    (cond ((> (length (param-description-list param)) 2)
           (setf (caddr (param-description-list param)) value))
          (param (setf (cddr (param-description-list param)) (list value)))
          (t (setf (cdr (last (parameter-definitions-tree params)))
                (list (new-param-node indicator :description value)))))
    value))


(defsetf description set-description)


(defun type-tag (params indicator)
  (let ((tag (car (get-param params indicator))))
    (if tag
        (intern (string-upcase (subseq tag 13 (search "Parameter" tag)))
                :keyword))))


(defun set-type-tag (params indicator value)
  (let ((param (get-param params indicator)))
    (if param
        (setf (car param) (param-tag-string value))
        (setf (cdr (last (parameter-definitions-tree params)))
              (list (new-param-node indicator :tag value))))
    value))


(defsetf type-tag set-type-tag)


(defun remparameter (params indicator)
  (do ((curr (cdr (xml-utils:find-first (slot-value params '.config)
                                        "parameterDefinitions"))
             (cdr curr))
       removed)
      ((endp (cdr curr)) removed)
    (when (eq (param-name (cadr curr)) indicator)
      (setf (cdr curr) (cddr curr)
            removed t))))
