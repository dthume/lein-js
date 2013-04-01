(ns lein-js.closure
  (:import [com.google.javascript.jscomp
            CompilerOptions JSSourceFile CompilationLevel WarningLevel
            ClosureCodingConvention DiagnosticGroups CheckLevel]
           [java.nio.charset Charset])
  (:use [clojure.string :only [lower-case]]
        [clojure.java.io :only [file]]))

(def default-options
     {:compilation-level :simple-optimizations
      :warning-level :default
      :coding-convention :default
      :summary-detail :print-on-errors
      :compilation-errors []
      :compilation-warnings ["FILEOVERVIEW_JSDOC" "UNKNOWN_DEFINES"]
      :compilation-ignored ["DEPRECATED" "VISIBILITY" "ACCESS_CONTROLS"
			    "STRICT_MODULE_DEP_CHECK" "MISSING_PROPERTIES"
			    "CHECK_TYPES"]
      :inline-constant-vars false
      :inline-local-variables false
      :inline-variables false
      :pretty-print false
      :print-input-delimiter false
      :process-closure-primitives false
      :manage-closure-deps false
      :charset "UTF-8"
      :define {}})

(def compilation-levels
     {:whitespace-only CompilationLevel/WHITESPACE_ONLY
      :simple-optimizations CompilationLevel/SIMPLE_OPTIMIZATIONS
      :advanced-optimizations CompilationLevel/ADVANCED_OPTIMIZATIONS})

(def warning-levels
     {:quiet WarningLevel/QUIET
      :default WarningLevel/DEFAULT
      :verbose WarningLevel/VERBOSE})

(def coding-conventions
     {:default ClosureCodingConvention
      :closure ClosureCodingConvention})

(def summary-details
     {:never-print 0
      :print-on-errors 1
      :print-if-type-checking 2
      :always-print 3})

(def diagnostic-groups (seq (.getFields DiagnosticGroups)))

(defn filter-fields
  [group-names]
  (let [name-set (set group-names)]
    (filter #(name-set (.getName %)) diagnostic-groups)))

(defn set-compilation-level
  [compiler-options level]
  (.setOptionsForCompilationLevel level compiler-options))

(defn set-warning-level
  [compiler-options level]
  (.setOptionsForWarningLevel level compiler-options))

(defn set-diagnostic
  [compiler-options groups level]
  (doseq [field (filter-fields groups)]
    (.setWarningLevel compiler-options (.get field nil) level)))

(defn set-compiler-option-fields
  [compiler-options user-options output]
  (doto compiler-options
    (.setCodingConvention (.newInstance ((:coding-convention user-options) coding-conventions)))
    (.setSummaryDetailLevel ((:summary-detail user-options) summary-details))
    (.setManageClosureDependencies (boolean (:manage-closure-deps user-options)))
    (.setInlineConstantVars (boolean (:inline-constant-vars user-options)))
    (.setInlineLocalVariables (boolean (:inline-local-variables user-options)))
    (.setInlineVariables (boolean (:inline-variables user-options))))
  
  ;; CompilerOptions has no setters defined for these fields
  (set! (. compiler-options prettyPrint) (:pretty-print user-options))
  (set! (. compiler-options printInputDelimiter) (:print-input-delimiter user-options))
  (set! (. compiler-options closurePass) (:process-closure-primitives user-options)))

;; See DiagnosticGroups.setWarningLevels
(defn set-diagnostics
  [compiler-options user-options]
  (doto compiler-options
      (set-diagnostic (:compilation-errors user-options) CheckLevel/ERROR)
      (set-diagnostic (:compilation-warnings user-options) CheckLevel/WARNING)
      (set-diagnostic (:compilation-ignored user-options) CheckLevel/OFF)))

(defn set-defines
  [opts u-options]
  (doseq [[kw v] (:define u-options)]
    (let [k (name kw)]
      (cond
       (instance? java.lang.Boolean v) (.setDefineToBooleanLiteral opts k v)
       (integer? v) (.setDefineToNumberLiteral opts k v)
       (number? v) (.setDefineToDoubleLiteral opts k v)
       :else (.setDefineToStringLiteral opts k (str v))))))

(defn make-compiler-options
  [options output]
  (let [user-options (merge default-options options)
	compiler-options (CompilerOptions.)]
    (doto compiler-options
      (set-compilation-level ((:compilation-level user-options) compilation-levels))
      (set-warning-level ((:warning-level user-options) warning-levels))
      (set-compiler-option-fields user-options output)
      (set-diagnostics user-options)
      (set-defines user-options))))

(defn write-output
  [compiler output]
  (println "Writing result to" (.getAbsolutePath output))
  (spit output (.toSource compiler)))

(defn run
  [inputs output options]
  (let [options (merge default-options options)
	compiler-options (make-compiler-options options output)
	charset (Charset/forName (:charset options))
	inputs (map #(JSSourceFile/fromFile % charset) inputs)
	externs (map #(JSSourceFile/fromFile % charset) (:externs options))
	compiler (com.google.javascript.jscomp.Compiler. System/err)]
    (com.google.javascript.jscomp.Compiler/setLoggingLevel java.util.logging.Level/WARNING)
    (.compile compiler
              (into-array JSSourceFile externs)
              (into-array JSSourceFile inputs)
              compiler-options)
    (write-output compiler (file output))))