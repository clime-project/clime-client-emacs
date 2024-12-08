(provide 'clime)

;;------------------------------------------------------------------------------
;;
;; customization
;;
;;------------------------------------------------------------------------------
(defgroup clime nil
  "clime."
  :group 'editing
  :prefix "clime-")

(defcustom clime-server     "localhost"      "" :type 'string  :group 'clime)
(defcustom clime-port              1178      "" :type 'integer :group 'clime) ; skkservと同じ
(defcustom clime-direct-keymap     '()       "" :type 'list    :group 'clime) ; ダイレクトキーマップ
(defcustom clime-predict-after-fix nil       "" :type 'boolean :group 'clime) ; 確定時自動候補検索
(defcustom clime-candidate-limit   30        "" :type 'integer :group 'clime) ; 候補数上限
(defcustom clime-indexing          nil       "" :type 'booelan :group 'clime) ; インデックス機能の初期状態
(defcustom clime-directkey         t         "" :type 'booelan :group 'clime) ; ダイレクトキー機能の初期状態
(defcustom clime-web-dict-url      ""        "" :type 'string  :group 'clime)

(defcustom clime-key-toggle        "C-j"     "" :type 'string  :group 'clime)
(defcustom clime-key-register      "C-o"     "" :type 'string  :group 'clime)
(defcustom clime-key-unregister    "C-q"     "" :type 'string  :group 'clime)
(defcustom clime-key-indexing      "C-i"     "" :type 'string  :group 'clime)
(defcustom clime-key-directkey     "C-d"     "" :type 'string  :group 'clime)
(defcustom clime-key-katakana      "C-k"     "" :type 'string  :group 'clime)
(defcustom clime-key-hiragana      "C-h"     "" :type 'string  :group 'clime)
(defcustom clime-key-ascii         "C-g"     "" :type 'string  :group 'clime)
(defcustom clime-key-insert-date   "C-;"     "" :type 'string  :group 'clime)
(defcustom clime-key-insert-time   "C-:"     "" :type 'string  :group 'clime)
(defcustom clime-key-websearch     "C-. C-w" "" :type 'string  :group 'clime)

;;(defvar clime-direct-keymap '((?. "。")
;;                              (?, "、")
;;                              (?[ "「")
;;                              (?] "」")))


;;------------------------------------------------------------------------------
;;
;; internal variables
;;
;;------------------------------------------------------------------------------
(defvar clime-current-buffer      nil)    ; clime が現在有効なバッファ
(defvar clime-pat                  "")    ; 検索パタン
(defvar clime-selection            "")    ; テンポラリに選択/表示されている文字列
(defvar clime-selection-area      nil)
(defvar clime-cands               nil)
(defvar clime-cands-count           0)
(defvar clime-cands-no-more       nil)    ; 継続検索の余地があるか否か
(defvar clime-sel-index             0)    ; いくつめの候補を選択しているか
(defvar clime-candidate-area      nil)
(defvar clime-exact               nil)
(defvar clime-original-local-map  nil)
(defvar clime-local-map           nil)    ; CLIME モードのキーマップ
(defvar clime-conv-local-map      nil)    ; パタン入力中のキーマップ
(defvar clime-modeline-fmt-bkup   nil)

;;------------------------------------------------------------------------------
;;
;; constants
;;
;;------------------------------------------------------------------------------
(defconst clime-exit-char       ?\033)
(defconst clime-delete-char     ?\177)
(defconst clime-kill-char       ?\^U)
(defconst clime-isearch-char    ?\^S)
(defconst clime-server-name     "climeserver")
(defconst clime-buffer-name     "*clime*")
(defconst clime-modeline-tag    "[CLIME]")


;;------------------------------------------------------------------------------
;;
;; internal functions
;;
;;------------------------------------------------------------------------------

(defun clime-make-range (&optional pt1 pt2)
  (let ((pt (point)))
    (unless pt1 (setq pt1 pt))
    (unless pt2 (setq pt2 pt))
    (cons pt1 pt2)))


(defun clime-clear-range (rng)
  (let ((pt (point)))
    (setf (car rng) pt)
    (setf (cdr rng) pt)
    rng))

(defmacro clime-start-of    (rng) `(car ,rng))
(defmacro clime-end-of      (rng) `(cdr ,rng))
(defmacro clime-range-empty (rng) `(= (car ,rng) (cdr ,rng)))

(defmacro clime-delete-region (rng)
  `(delete-region (clime-start-of ,rng)
                  (clime-end-of   ,rng)))

(defun clime-join-strings (list delim)
  "list の全要素文字列を delim で繋いだ文字列を返す"
  (cl-labels ((recur (acc rest)
                (if (null rest)
                    acc
                    (recur (concat acc delim (car rest)) (cdr rest)))))
    (recur (car list) (cdr list))))

(defun clime-get-top-line-of-buffer (buf-name)
  "指定されたバッファの「１行目のテキスト」を文字列で返す。"
  (with-current-buffer buf-name
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "\n" nil t))
    (let ((top (point-min)))
      (goto-char top)
      (search-forward "\n" nil t)
      (buffer-substring top (1- (point))))))

(defun clime-listify-search-result (response)
  "検索のレスポンスを単語リストに変換する : \\='1@word1@word2@\\=' => (\\='word1\\=' \\='word2\\=')"
  (when (= ?1 (aref response 0))
    (let ((delim (aref response 1))
          (max   (length response)))
      (cl-labels ((recur (cur acc)
                    (let ((end (cl-position delim response :start cur)))
                      (if (null end)
                          (values (nreverse acc) nil)
                          (if (= cur end)
                              (values (nreverse acc) t)    ;; t means 'no more.'
                              (recur (1+ end) (cons (cl-subseq response cur end) acc)))))))
        (recur 2 nil)))))

(defun clime-send-command-and-wait (cmd)
  (with-current-buffer clime-buffer-name
    (erase-buffer)
    (process-send-string clime-server-name cmd)
    (let ((cont t))
      (while (and cont (process-status clime-server-name))
        (accept-process-output)
        (when (< 0 (buffer-size))
          (setq cont nil))))
    (erase-buffer)))

(defun clime-ins-cand-without-index (idx str)
  (insert " " str))

(defun clime-ins-cand-with-index (idx str)
  (insert (if (<= 9 idx)
              "  " 
              (format " %d:" (1+ idx))) str))

(defun clime-nth-candidate (idx)
  "idx 番目の候補を返す（最初の候補は0番目）"
  (or (nth idx clime-cands) ""))

(defun clime-display-candidates ()
  "候補リストをカーソルの1行下に表示する"
  (save-excursion
    (goto-char (clime-start-of clime-selection-area))
    (let ((col (current-column)))
      (end-of-line)
      (setf (clime-start-of clime-candidate-area) (point))
      (insert "\n")
      (insert (make-string col 32)))
    (let ((idx  0)
          (max  10)
          (base clime-sel-index)
          (fnc  (if clime-indexing #'clime-ins-cand-with-index
                                   #'clime-ins-cand-without-index)))
      (while (and (< idx max) (nth (+ base idx) clime-cands))
        (funcall fnc idx (clime-nth-candidate (+ base idx)))
        (cl-incf idx)))
    (setf (clime-end-of clime-candidate-area) (point))))

(defun clime-erase-candidates ()
  "候補リスト表示を消す"
  (unless (clime-range-empty clime-candidate-area)
    (clime-delete-region clime-candidate-area))
  (clime-clear-range clime-candidate-area))

(defun clime-reset ()
  (setq clime-pat               "")
  (setq clime-cands             nil)
  (setq clime-cands-count       0)
  (setq clime-cands-no-more     nil)
  (setq clime-sel-index         0)
  (setq clime-selection         "")
  (setq clime-selection-area    (clime-make-range))
  (setq clime-candidate-area    (clime-make-range))
  (setq clime-exact             nil))

(defun clime-search-impl (request)
  (with-current-buffer clime-buffer-name
    (erase-buffer)
    (setq truncate-lines t)
    (process-send-string clime-server-name request)
    (let ((cont t))
      (while (and cont (process-status clime-server-name))
        (accept-process-output)
        (when (< 0 (buffer-size))
          (if (eq (char-after 1) ?1)
              ;; found key successfully, so check if a whole line is received.
              (when (eq (char-after (1- (point-max))) ?\n)
                (setq cont nil))
            (setq cont nil)))))
    (let ((response (clime-get-top-line-of-buffer clime-buffer-name)))
      (clime-listify-search-result response))))

(defun clime-search (pat &optional exact)
  (clime-search-impl (format "%c%s\n" (if exact ?X ?S) pat)))

(defun clime-search-next ()
  (clime-search-impl "N\n"))

(defun clime-erase-selection ()
  "入力中のパターン（または選択中の候補）を消す"
  (unless (clime-range-empty clime-selection-area)
    (clime-delete-region clime-selection-area))
  (clime-clear-range clime-selection-area))

(defun clime-set-selection (str)
  "入力パターン（または選択中の候補）を設定して表示"
  (clime-erase-selection)
  (setq clime-selection str)
  (insert "[" clime-selection "]")
  (setf (clime-end-of clime-selection-area) (point)))

(defun clime-paren-pair-p (data)
  (and (= 2 (length data))
       (or (string= data "「」")
           (string= data "『』")
           (string= data "〈〉")
           (string= data "【】")
           (string= data "《》")
           (string= data "〔〕")
           (string= data "＜＞")
           (string= data "（）")
           (string= data "［］")
           (string= data "｛｝")
           (string= data "‘’")
           (string= data "“”"))))

;; 候補をcandに確定
(defun clime-fix (cand)
  (clime-erase-candidates)
  (clime-erase-selection)
  (insert cand)
  (when (clime-paren-pair-p cand)
    (backward-char))
  (clime-reset))

(defun clime-selcand ()
  (clime-erase-candidates)
  (clime-set-selection (clime-nth-candidate clime-sel-index))
  (cl-incf clime-sel-index)
  (clime-display-candidates))

(defun clime-prevcand ()
  (when (< 0 clime-sel-index)
    (clime-erase-candidates)
    (cl-decf clime-sel-index)
    (clime-set-selection (if (< 0 clime-sel-index)
                           (clime-nth-candidate (1- clime-sel-index))
                           clime-pat))
    (clime-display-candidates)))

(defun clime-re-search (pat)
  (multiple-value-bind (entries no-more) (clime-search pat nil)
    (let ((ptn (clime-join-strings entries "\\|")))
      (re-search-forward ptn nil t))))

(defun clime-set-candlimit (limit)
  (clime-send-command-and-wait (format "R%d\n" limit)))

(defun clime-setcontext (word)
  (clime-send-command-and-wait (format "4%s\n" word)))

(defun clime-register (word pat)
  (cl-labels ((decide-delim (lst)
                (if (and (not (cl-find (car lst)  pat))
                         (not (cl-find (car lst) word)))
                    (car lst)
                  (decide-delim (cdr lst)))))
    (let ((delim (decide-delim '(?| ?, ?! ?# ?$ ?% ?& ?' ?* ?+
                                 ?- ?. ?/ ?: ?= ?? ?^ ?_ ?` ?~ ?\"))))
      (clime-send-command-and-wait (format "T%c%s%c%s\n" delim pat delim word)))))

(defun clime-unregister (word)
  (clime-send-command-and-wait (format "U%s\n" word)))

(defun clime-save-dict ()
  (clime-send-command-and-wait "7\n"))

(defun clime-notifyselect (idx)
  (when (<= 0 idx)
    (clime-send-command-and-wait (format "8%d\n" idx))))

(defun clime-insert-datetime-impl (pattern)
  (use-local-map clime-conv-local-map)
  (setq case-fold-search nil)
  (if (< 0 clime-sel-index)
      (progn
        ;; 候補選択中なら確定
        (clime-notifyselect (1- clime-sel-index))
        (clime-fix clime-selection))
      (progn
        (clime-erase-candidates)
        (clime-fix (clime-nth-candidate clime-sel-index))))
  (when clime-exact
    (clime-fix clime-selection))
  (setq clime-pat pattern)
  (clime-set-selection clime-pat)
  (multiple-value-bind (entries no-more) (clime-search clime-pat)
    (setq clime-cands         entries)
    (setq clime-cands-count   (length entries))
    (setq clime-cands-no-more no-more))
  (clime-display-candidates))


;;------------------------------------------------------------------------------
;;
;; interactive commands
;;
;;------------------------------------------------------------------------------
(defun clime-register-word (arg)
  (interactive "i")
  (if (zerop (length clime-pat))
      (let* ((word (buffer-substring (mark) (point)))
             (pat  (read-from-minibuffer
                    (concat "単語登録:「" word "」の読み(ローマ字): "))))
        (when (< 0 (length pat))
          (clime-register word pat)))
      (progn
        (let ((word (read-from-minibuffer
                     (concat "単語登録:パターン「" clime-pat "」の単語: "))))
          (when (< 0 (length word))
            (clime-register word clime-pat)
            (clime-erase-candidates)
            (clime-erase-selection)
            (insert word)
            (clime-reset)
            (use-local-map clime-local-map))))))

(defun clime-space (arg)
  (interactive "P")
  (when (or clime-cands (< 0 (length clime-pat)))
    (clime-selcand)
    (when (and (not clime-cands-no-more)
               (< clime-cands-count (+ clime-sel-index 10)))
      (clime-erase-candidates)
      (multiple-value-bind (entries no-more) (clime-search-next)
        (setq clime-cands         entries)
        (setq clime-cands-count   (length entries))
        (setq clime-cands-no-more no-more))
      (clime-display-candidates))))

(defun clime-ascii-fix (arg)
  (interactive "P")
  (clime-erase-candidates)
  (clime-erase-selection)
  (insert clime-pat)
  (clime-reset)
  (use-local-map clime-local-map))

(defun clime-katakana-fix (arg)
  (interactive "P")
  (multiple-value-bind (entries no-more) (clime-search clime-pat t)
    (setq clime-cands         entries)
    (setq clime-cands-count   (length entries))
    (setq clime-cands-no-more no-more))
  (clime-notifyselect 1)
  (clime-fix (clime-nth-candidate 1))
  (use-local-map clime-local-map))

(defun clime-hiragana-fix (arg)
  (interactive "P")
  (multiple-value-bind (entries no-more) (clime-search clime-pat t)
    (setq clime-cands         entries)
    (setq clime-cands-count   (length entries))
    (setq clime-cands-no-more no-more))
  (clime-notifyselect 0)
  (clime-fix (clime-nth-candidate 0))
  (use-local-map clime-local-map))

(defun clime-unregister-word (arg)
  (interactive "P")
  (clime-unregister clime-selection)
  (clime-selcand))

(defun clime-toggle-indexing (arg)
  (interactive "P")
  (clime-erase-candidates)
  (setq clime-indexing (not clime-indexing))
  (clime-display-candidates)
  (message "clime index feature %s." (if clime-indexing "enabled" "disabled")))

(defun clime-toggle-directkey (arg)
  (interactive "P")
  (setq clime-directkey (not clime-directkey))
  (message "clime directkey feature %s." (if clime-directkey "enabled" "disabled")))

;;語句の定義を web 検索する：テンプレートは clime-web-dict-url 変数
(defun clime-definition-web (arg)
  (interactive "P")
  ;;「語句」は、変換中であれば選択中の候補、それ以外の場合はリージョン
  (let ((word (if (zerop (length clime-pat))
                  (buffer-substring (mark) (point))
                (clime-nth-candidate (1- clime-sel-index)))))
    (when (< 0 (length word))
      (let* ((default (replace-regexp-in-string "~A" word
                                                clime-web-dict-url))
             (url     (if (not arg)
                          default
                        (read-from-minibuffer "URL: " default))))
        ;;MEMO : 空文字列でなければブラウザ起動
        (unless (string= url "")
          (browse-url url))))))

(defun clime-del (arg)
  (interactive "P")
  (let ((len (length clime-pat)))
    (if (zerop len)
        (progn
          (clime-erase-candidates)
          (clime-erase-selection)
          (clime-reset)
          (backward-delete-char 1)
          (use-local-map clime-local-map))
        (if (< 0 clime-sel-index)
            (clime-prevcand)
            (progn
              (clime-erase-candidates)
              (setq clime-pat (substring clime-pat 0 (1- len)))
              (clime-set-selection clime-pat)
              (multiple-value-bind (entries no-more) (clime-search clime-pat)
                (setq clime-cands         entries)
                (setq clime-cands-count   (length entries))
                (setq clime-cands-no-more no-more))
              (clime-display-candidates))))))

(defun clime-isearch ()
  (interactive)
  (cl-labels ((show-message (&optional str)
                (let ((s "CLIME-Isearch: "))
                  (when (stringp str)
                    (setq s (concat s str)))
                  (message s))))
    (show-message)
    (let ((more t)
          (ptn "")
          (pos (point)))
      (while more
        (let ((char (read-char)))
          (cond
           ((eq char clime-exit-char)
            (goto-char pos)
            (setq more nil))
           ((eq char clime-delete-char)
            (if (> (length ptn) 0)
                (setq ptn (substring ptn 0 (1- (length ptn)))))
            (goto-char pos)
            (clime-re-search ptn))
           ((eq char clime-kill-char)
            (setq ptn "")
            (show-message))
           ((eq char clime-isearch-char)
            (clime-re-search ptn))
           ((< char 32)
            (setq more nil)
            (setq unread-command-events char))
           (t
            (setq ptn (concat ptn (char-to-string char)))
            (goto-char pos)
            (clime-re-search ptn)))
          (show-message ptn))))))

(defun clime-newline (arg)
  (interactive "P")
  (if (and clime-cands (< 0 clime-sel-index))
      ;;変換中＆候補選択中の場合
      (progn
        (clime-notifyselect (1- clime-sel-index))
        (clime-fix clime-selection)
        (if clime-predict-after-fix
            (multiple-value-bind (entries no-more) (clime-search clime-pat)
              (setq clime-cands         entries)
              (setq clime-cands-count   (length entries))
              (setq clime-cands-no-more no-more)
              (clime-display-candidates))
            (use-local-map clime-local-map)))
      ;;上記以外
      (if (not (string= clime-pat ""))
          (if clime-exact
              (progn
                (clime-notifyselect (1- clime-sel-index))
                (clime-fix clime-selection)
                (use-local-map clime-local-map))
              (progn
                ;; 完全マッチモードに移行
                (clime-erase-candidates)
                (setq clime-exact t)
                (multiple-value-bind (entries no-more) (clime-search clime-pat t) ; 完全マッチを検索
                  (setq clime-cands         entries)
                  (setq clime-cands-count   (length entries))
                  (setq clime-cands-no-more no-more))
                (clime-set-selection (clime-nth-candidate 0))
                (cl-incf clime-sel-index)
                (clime-display-candidates)))
          (if (and clime-cands (< 0 clime-sel-index))
              (progn
                (clime-notifyselect (1- clime-sel-index))
                (clime-fix clime-pat)
                (use-local-map clime-local-map))
              (progn
                (clime-erase-candidates)
                (use-local-map clime-local-map)
                (newline))))))

(defun clime-insert-date (arg)
  (interactive "P")
  (let ((dt (decode-time)))
    (clime-insert-datetime-impl (format "%d/%d/%d" (nth 5 dt) (nth 4 dt) (nth 3 dt)))))

(defun clime-insert-time (arg)
  (interactive "P")
  (let ((dt (decode-time)))
    (clime-insert-datetime-impl (format "%d:%d:%d" (nth 2 dt) (nth 1 dt) (nth 0 dt)))))

(defun clime-keyin (arg)
  (interactive "P")
  (let ((evt last-input-event))
    (if (and clime-indexing clime-cands (<= ?1 evt) (<= evt ?9))
        ;;インデックス有効でインデックスキー（1～9）が押下された場合
        (let ((idx (+ clime-sel-index (- evt ?1))))
          (setq clime-selection (clime-nth-candidate idx))
          (clime-notifyselect idx)
          (clime-fix clime-selection)
          (if clime-predict-after-fix
              (multiple-value-bind (entries no-more) (clime-search clime-pat)
                (setq clime-cands         entries)
                (setq clime-cands-count   (length entries))
                (setq clime-cands-no-more no-more)
                (clime-display-candidates))
              (use-local-map clime-local-map)))
        ;;上記以外の場合
        (let ((dkey (assoc evt clime-direct-keymap)))
          (if (and clime-directkey dkey)
              ;; direct-key 有効で、direct-key のいずれかが押下された場合
              (progn
                (when (and clime-cands (< 0 clime-sel-index))
                  (clime-notifyselect (1- clime-sel-index)))
                (clime-fix clime-selection)
                (insert (cadr dkey))
                (clime-setcontext (cadr dkey))
                (if clime-predict-after-fix
                    (multiple-value-bind (entries no-more) (clime-search clime-pat)
                      (setq clime-cands         entries)
                      (setq clime-cands-count   (length entries))
                      (setq clime-cands-no-more no-more)
                      (clime-display-candidates))
                    (use-local-map clime-local-map)))
              ;; 上記以外の場合
              (progn
                (use-local-map clime-conv-local-map)
                (setq case-fold-search nil)
                (if (< 0 clime-sel-index)
                    (progn
                      ;; 候補選択中なら確定
                      (clime-notifyselect (1- clime-sel-index))
                      (clime-fix clime-selection))
                    (progn
                      (clime-erase-candidates)
                      (unless (and (<= ?! evt) (<= evt ?~))
                        (clime-fix (clime-nth-candidate clime-sel-index)))))
                (when clime-exact
                  (clime-fix clime-selection))
                (setq clime-pat (concat clime-pat (char-to-string evt)))
                (clime-set-selection clime-pat)
                (multiple-value-bind (entries no-more) (clime-search clime-pat)
                  (setq clime-cands         entries)
                  (setq clime-cands-count   (length entries))
                  (setq clime-cands-no-more no-more)
                  (clime-display-candidates))))))))


(defun clime-finish (arg)
  (interactive "P")
  (condition-case err
      (progn
        (when (and clime-cands (< 0 clime-sel-index))
          (clime-notifyselect (1- clime-sel-index)))
        (clime-fix clime-selection)
        (clime-save-dict)
        (clime-erase-candidates)
        (process-send-string clime-server-name "0\n") ; サーバから切断
        (delete-process clime-server-name))
    (error (prog1 nil
             (message "error : clime server not found."))))
  (use-local-map clime-original-local-map)
  (setq mode-line-format clime-modeline-fmt-bkup)
  (clime-reset)
  (setq clime-current-buffer    nil)
  (remove-hook 'kill-buffer-hook 'clime-kill-buffer-hook)
  (remove-hook 'kill-emacs-hook  'clime-kill-buffer-hook))

(defun clime-kill-buffer-hook ()
  (when (eq (current-buffer) clime-current-buffer)
    (with-current-buffer clime-current-buffer
      (clime-finish nil))))

(defun clime-kill-emacs-hook ()
  (when clime-current-buffer
    (with-current-buffer clime-current-buffer
      (clime-finish nil))))

;;------------------------------------------------------------------------------
;;
;; pobox mode
;;
;;------------------------------------------------------------------------------
(defun clime-mode (arg)
  (interactive "P")

  ;; 別バッファで clime 使用中だった場合は先にそちらを終了
  (when clime-current-buffer
    (with-current-buffer clime-current-buffer
      (clime-finish nil)))

  (clime-reset)
  
  ;; サーバとの通信路/バッファの初期化
  (unless (eq (process-status clime-server-name) 'open)
    (save-excursion
      (get-buffer-create clime-buffer-name)
      (set-buffer clime-buffer-name)
      (let ((process (condition-case err
                         (open-network-stream clime-server-name
                                              clime-buffer-name clime-server clime-port)
                       (error nil))))
        (if (null process)
            (message "error : clime server not found.")
            (progn
              (set-process-query-on-exit-flag process nil)
              (set-process-coding-system process 'utf-8-unix 'utf-8-unix))))))

  (when (eq (process-status clime-server-name) 'open)

    ;; 対象バッファとモード行の設定
    (setq clime-current-buffer    (current-buffer))
    (setq clime-modeline-fmt-bkup mode-line-format)
    (setq mode-line-format (cons clime-modeline-tag (cdr mode-line-format)))
    (add-hook 'kill-buffer-hook 'clime-kill-buffer-hook)
    (add-hook 'kill-emacs-hook  'clime-kill-emacs-hook)

    ;; キーマップの設定
    (setq clime-original-local-map (current-local-map))
    (if (null clime-original-local-map)
        (setq clime-local-map (make-keymap))
      (setq clime-local-map (copy-keymap (current-local-map))))
    (define-key clime-local-map (kbd clime-key-toggle)      'clime-finish)
    (define-key clime-local-map (kbd "C-s")                 'clime-isearch)
    (define-key clime-local-map (kbd clime-key-register)    'clime-register-word)
    (define-key clime-local-map (kbd clime-key-insert-date) 'clime-insert-date)
    (define-key clime-local-map (kbd clime-key-insert-time) 'clime-insert-time)
    (define-key clime-local-map (kbd clime-key-websearch)   'clime-definition-web)
    
    ;; 変換中のキーマップの設定
    (let ((keymap (make-keymap)))
      (dotimes (i 128)
        (define-key keymap (char-to-string i)   'undefined))
      (define-key keymap (kbd "C-m")                 'clime-newline)
      (define-key keymap (kbd "SPC")                 'clime-space)
      (define-key keymap (kbd "DEL")                 'clime-del)
      (define-key keymap (kbd clime-key-register)    'clime-register-word)
      (define-key keymap (kbd clime-key-unregister)  'clime-unregister-word)
      (define-key keymap (kbd clime-key-indexing)    'clime-toggle-indexing)
      (define-key keymap (kbd clime-key-directkey)   'clime-toggle-directkey)
      (define-key keymap (kbd clime-key-hiragana)    'clime-hiragana-fix)
      (define-key keymap (kbd clime-key-katakana)    'clime-katakana-fix)
      (define-key keymap (kbd clime-key-ascii)       'clime-ascii-fix)
      (define-key keymap (kbd clime-key-insert-date) 'clime-insert-date)
      (define-key keymap (kbd clime-key-insert-time) 'clime-insert-time)
      (define-key keymap (kbd clime-key-websearch)   'clime-definition-web)
      (setq clime-conv-local-map keymap))
    
    ;; a,b,...などの文字に対し clime-keyin を割りあてる
    (let ((i 33))
      (while (< i 127)
        (define-key clime-local-map      (char-to-string i) 'clime-keyin)
        (define-key clime-conv-local-map (char-to-string i) 'clime-keyin)
        (cl-incf i)))

    (use-local-map clime-local-map)

    (when clime-candidate-limit
      (clime-set-candlimit clime-candidate-limit))))
