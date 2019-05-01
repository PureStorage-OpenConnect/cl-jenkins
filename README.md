# cl-jenkins

A Common Lisp library for interacting with Jenkins.  It's been used
with SBCL, and it ought to work with any conforming implementation.

This library was designed/tested to work with Jenkins 1.x.  Some
functionality may still work with Jenkins 2.x, but it's highly
unlikely that everything will work with Jenkins 2 unmodified.

## Setup

The best way to install this library is via
[Quicklisp](https://www.quicklisp.org/beta/).  Follow the instructions
on the linked page to set it up, if you haven't already.

With Quicklisp:

```common-lisp
(load "/path/to/your/cl-jenkins/cl-jenkins.asd")
;=> T

(ql:quickload :cl-jenkins)
; To load "cl-jenkins":
;   Load 1 ASDF system:
;     cl-jenkins
; ; Loading "cl-jenkins"
; ..................................................
; [package xml-utils]...............................
; [package label-matcher]...........................
; [package cl-jenkins]...............
;=> (:CL-JENKINS)
```

cl-jenkins uses [Drakma](https://edicl.github.io/drakma/) under the
covers for making HTTP requests.  You may need to add
`application/json` and `application/xml` to Drakma's list of known
text content types:

```common-lisp
(push (cons "application" "json") drakma:*text-content-types*)
;=> (("application" . "json") ("text"))
(push (cons "application" "xml") drakma:*text-content-types*)
;=> (("application" . "xml") ("application" . "json") ("text"))
```

This should be enough to get the library up and serviceable.

## Usage

**NB:** This section is very much a WIP.  Sorry 'bout that.

First, you'll want to instantiate an object for your Jenkins instance:

```common-lisp
(defvar *jenkins*
        (make-instance 'cl-jenkins:jenkins
                       :port 8080
                       :protocol "https"
                       :name "jenkins"
                       :host "jenkins.example.com"
                       :auth '("flimby" "hunter2")))
;=> #<CL-JENKINS:JENKINS {10047DB233}>
```

It may be handy to have a Jenkins "registry" where you keep
information about all of your Jenkinses, and then rely on that to
populate the parameters to `MAKE-INSTANCE`.  This is what we do at
Pure.

### Working with Jobs

You can ask for a list of all jobs on a Jenkins:

```common-lisp
(cl-jenkins:jobs *jenkins*)
;=> (#<CL-JENKINS:JENKINS-JOB {1004C02893}> #<CL-JENKINS:JENKINS-JOB {1004C02E73}> #<CL-JENKINS:JENKINS-JOB {1004C03093}>)
```

Or for a specific job in particular:

```common-lisp
(cl-jenkins:job *jenkins* "some_jenkins_job")
;=> #<CL-JENKINS:JENKINS-JOB {1004855173}>
```

If there is no job on your Jenkins named what you asked for,
`CL-JENKINS:JOB` simply gives back `NIL`:

```common-lisp
(cl-jenkins:job *jenkins* "non_existent_job")
;=> NIL
```

Get the build parameters for a job as a property list
('NAMES_LIKE_THIS' are converted to 'NAMES-LIKE-THIS'):

```common-lisp
(cl-jenkins:params (cl-jenkins:job *jenkins* "some_jenkins_job"))
;=> (:A-PARAMETER "a default value" :ANOTHER-PARAMETER "another default" :BOOLEAN-PARAMETER T)
```

Kick off a build of a job (inverse to above, 'THESE-NAMES' will be
converted to 'THESE_NAMES'):

```common-lisp
(cl-jenkins:build-with-parameters (cl-jenkins:job *jenkins* "some_jenkins_job")
                                  :a-parameter "value for this build"
                                  :another-parameter "anotehr value for this build"
                                  :boolean-parameter nil)
;=> NIL
; 201
; ((:SERVER . "Apache-Coyote/1.1") (:X-CONTENT-TYPE-OPTIONS . "nosniff")
;  (:LOCATION . "https://jenkins.example.com:8080/queue/item/1825987/")
;  (:CONTENT-LENGTH . "0") (:DATE . "Wed, 01 May 2019 18:07:38 GMT")
;  (:CONNECTION . "close"))
; #<PURI:URI https://jenkins.example.com:8080/job/some_jenkins_job/buildWithParameters>
; #<FLEXI-STREAMS:FLEXI-IO-STREAM {10069E25E3}>
; T
; "Created"
```

Get a job's config.xml:


```common-lisp
(cl-jenkins:config (cl-jenkins:job *jenkins* "some_jenkins_job"))
;=> "<?xml version='1.0' encoding='UTF-8'?>
; <project>
;   <actions/>
;   <description>A job that does some testing.</description>
;   ...
; </project>"
```

You can create new jobs or updating existing ones just by naming them
and giving a valid config.xml for them:

```common-lisp
(cl-jenkins:create-or-update-job *jenkins*
                                 "a_new_jenkins_job"
                                 (cl-jenkins:config (cl-jenkins:job *jenkins*
                                                                    "some_jenkins_job")))
;=> #<CL-JENKINS:JENKINS-JOB {10074A3513}>
```

You can use this to modify jobs, copy them around on a Jenkins, or
even between Jenkinses.

## Possible Caveats

* As mentioned in the intro, this library has been tested with with
  Jenkins 1.x (specifically a few versions around 1.6xx.x).  It's
  quite possible that you'll need to make some edits for it to work
  with e.g. Jenkins 2.x
* Some of the functionality in the library relies on certain plugins
  being installed.  Notably, some of the API for dealing with labels
  expects the Node and Label parameter plugin, though it should mostly
  degrade gracefully to using the built in assignedNode if you don't
  have the plugin installed.
* This library is a port of a Python library we use internally at
  Pure.  We tried to make it Lispy whenever we could, sorry if the API
  seems a bit foreign to a Lisp programmer.
