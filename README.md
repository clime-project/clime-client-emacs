# clime-client-emacs

　CLIME の Emacs エディタ向けクライアントです。


## インストール

　clime.el を load-path で指定されているロードパスのどこかに配備してください
（byte compile はご自由に）。そして emacs.el などの Emacs 初期化ファイルに
以下を追加します。

```
 (require 'clime)
 (autoload 'clime-mode "clime.el")
 (setq clime-key-toggle "C-\\")
 (global-set-key (kbd clime-key-toggle) 'clime-mode)
 (setq clime-server "localhost")
 (setq clime-port 1179)
 (setq clime-candidate-limit 30)
 (setq clime-indexing t)
 (setq clime-predict-after-fix t)
 (setq clime-key-insert-date "C-;")
 (setq clime-key-insert-time "C-:")
 (setq clime-web-dict-url "https://www.google.com/search?q=辞書+~A")
 (setq clime-direct-keymap '((?, "、")
                             (?. "。")
                             (?[ "「")
                             (?] "」")
                             (?( "（")
                             (?) "）")
                             (?! "！")
                             (?? "？")
                             (?< "＜")
                             (?> "＞")))
```


## 使い方

　日本語入力を行ないたいバッファで `C-_` を押下することで CLIME マイナーモードを 
on/off できます。on の状態で文字入力をすると、以下のように候補が表示されます。
スペースキーを繰り返し押すことで候補を選択して Enter キーで確定するか、1 ～ 9 の
インデックスキーで直接選択してください。

```
CLIME での入力[rei]
               1:例 2:礼金 3:〇 4:０ 5:霊安室 6:レイアウト 7:冷罵 8:冷媒 9:霊廟  冷房
```

　以下の機能については Windows クライアントと（ほぼ）同等の機能を実装しています。

* CLIME による日本語入力 : `C-_` で on/off
* インデックス機能 : 変換中に `C-i` で on/off
* ダイレクトキー機能 : 変換中に `C-d` で on/off
* 単語登録機能 : 変換中に `C-o` で単語登録
* 単語削除機能 : 変換中に `C-q` で単語削除
