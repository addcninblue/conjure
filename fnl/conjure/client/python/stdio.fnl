(module conjure.client.python.stdio
  {autoload {a conjure.aniseed.core
             extract conjure.extract
             str conjure.aniseed.string
             nvim conjure.aniseed.nvim
             stdio conjure.remote.stdio
             config conjure.config
             text conjure.text
             mapping conjure.mapping
             client conjure.client
             log conjure.log
             ts conjure.tree-sitter}
   require-macros [conjure.macros]})

; TODO prompt_pattern for julia seems to not show, empty "" is problematic.
(config.merge
  {:client
   {:python
    {:stdio
     {:mapping {:start "cs"
                :stop "cS"
                :interrupt "ei"}
      :command "ipython"  ; TODO: make better interpreter
      :prompt_pattern ">>>"}}}})

(def- cfg (config.get-in-fn [:client :python :stdio]))

(defonce- state (client.new-state #(do {:repl nil})))

(def buf-suffix ".py")
(def comment-prefix "# ")

(defn- with-repl-or-warn [f opts]
  (let [repl (state :repl)]
    (if repl
      (f repl)
      (log.append [(.. comment-prefix "No REPL running")
                   (.. comment-prefix
                       "Start REPL with "
                       (config.get-in [:mapping :prefix])
                       (cfg [:mapping :start]))]))))

; TODO: prep-code?
(defn- prep-code [s]
  (.. s "\n\n"))
  ; (.. "exec(r\"\"\"" s "\"\"\")\n"))
  ; (.. s "\nif(isnothing(ans)) display(nothing) end\n"))

(defn unbatch [msgs]
  (->> msgs
       (a.map #(or (a.get $1 :out) (a.get $1 :err)))
       (str.join "\n")))

; TODO: fix
(defn format-msg [msg]
  ; remove last "nothing" if preceded by character or newline.
  (->> (-> (string.gsub msg "(.?[%w\n])(nothing)" "%1")
           (str.split "\n"))
       (a.filter #(~= "" $1))))

; TODO: fix
(defn form-node? [node]
  (-> node ts.node->str (string.match "julia>") not)
  )

(defn eval-str [opts]
  (with-repl-or-warn
    (fn [repl]
      (repl.send
        (prep-code opts.code)
        (fn [msgs]
          (let [msgs (-> msgs unbatch format-msg)]
             (log.append msgs)
             (when opts.on-result
               (opts.on-result (str.join " " msgs)))))
        {:batch? true}))))

(defn eval-file [opts]
  (eval-str (a.assoc opts :code (a.slurp opts.file-path))))

(defn doc-str [opts]
  (log.append (.. comment-prefix "doc-str is not implemented for Python")))

(defn- display-repl-status [status]
  (let [repl (state :repl)]
    (when repl
      (log.append
        [(.. comment-prefix (a.pr-str (a.get-in repl [:opts :cmd])) " (" status ")")]
        {:break? true}))))

(defn stop []
  (let [repl (state :repl)]
    (when repl
      (repl.destroy)
      (display-repl-status :stopped)
      (a.assoc (state) :repl nil))))

(defn start []
  (if (state :repl)
    (log.append [(.. comment-prefix "Can't start, REPL is already running.")
                 (.. comment-prefix "Stop the REPL with "
                     (config.get-in [:mapping :prefix])
                     (cfg [:mapping :stop]))]
                {:break? true})
    (a.assoc
      (state) :repl
      (stdio.start
        {:prompt-pattern (cfg [:prompt_pattern])
         :cmd (cfg [:command])

         :on-success
         (fn []
           (display-repl-status :started)
           (comment (with-repl-or-warn
              (fn [repl]
                (repl.send
                  (prep-code "using REPL")
                  (fn [msgs]
                    (log.append (-> msgs unbatch format-msg)))
                  {:batch? true})))))

         :on-error
         (fn [err]
           (display-repl-status err))

         :on-exit
         (fn [code signal]
           (when (and (= :number (type code)) (> code 0))
             (log.append [(.. comment-prefix "process exited with code " code)]))
           (when (and (= :number (type signal)) (> signal 0))
             (log.append [(.. comment-prefix "process exited with signal " signal)]))
           (stop))

         :on-stray-output
         (fn [msg]
           (log.append (-> [msg] unbatch format-msg) {:join-first? true}))}))))

(defn on-load []
  (start))

(defn on-exit []
  (stop))

(defn interrupt []
  (with-repl-or-warn
    (fn [repl]
      (let [uv vim.loop]
        (uv.kill repl.pid uv.constants.SIGINT)))))

(defn on-filetype []
  (mapping.buf :n :JuliaStart (cfg [:mapping :start]) *module-name* :start)
  (mapping.buf :n :JuliaStop (cfg [:mapping :stop]) *module-name* :stop)
  (mapping.buf :n :JuliaInterrupt (cfg [:mapping :interrupt]) *module-name* :interrupt))
