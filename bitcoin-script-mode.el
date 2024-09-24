;;; bitcoin-script-mode.el --- major mode for Bitcoin Scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024 Kyrylo Baibula

;; Author: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; Maintainer: Kyrylo Baibula <kyrylobaybula@gmail.com>
;; Homepage: https://github.com/Velnbur/bitcoin-script-mode
;; Keywords: bitcoin bitcoin-script

;; Package-Version: 0.4.3
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (company "1.0.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for Bitcoin Scripts which implements hightlighting and
;; basic formatting.

;;; Code:


(require 'cl-lib)
(require 'dash)
(require 'company)

(cl-defstruct btc-opcode
  word code input output description)

(defconst bitcoin-script-mode--disabled-opcodes
  '("OP_CAT" "OP_SUBSTR" "OP_LEFT" "OP_RIGHT" "OP_INVERT"
    "OP_AND" "OP_OR" "OP_XOR" "OP_2MUL" "OP_2DIV" "OP_MUL"
    "OP_DIV" "OP_MOD" "OP_LSHIFT" "OP_RSHIFT")
  "List of disabled Bitcoin opcodes.")

(cl-defun btc-opcode-disabled? (opcode)
  "Return hex encoded code of the opcode"
  (member (btc-opcode-word opcode) 'bitcoin-script-mode--disabled-opcodes))

(cl-defun btc-opcode-from-list (lst)
  "Return opcode from list `LST'."
  (make-btc-opcode
   :word        (nth 0 lst)
   :code        (nth 1 lst)
   :input       (nth 2 lst)
   :output      (nth 3 lst)
   :description (nth 4 lst)))

(cl-defun bit-opcode-eldoc-string (opcode)
  "Return a formatted doc string for eldoc frontend."
  (format "Code: %d (0x%X), Input: %s, Output: %s, %s"
	  (btc-opcode-code        opcode)
	  (btc-opcode-code        opcode)
	  (btc-opcode-input       opcode)
	  (btc-opcode-output      opcode)
	  (btc-opcode-description opcode)))

(defconst bitcoin-script-mode--pushnum-opcodes
  (-flatten
   (mapcar
    (lambda (num)
      (let ((word (format "OP_PUSHNUM_%d" num))
	    (word-short (format "OP_%d" num))
	    (make-opcode (lambda (num word)
			   (make-btc-opcode
			    :word word
			    :code (+ num 80)
			    :input "None"
			    :output (format "%d" num)
			    :description (format "The number %d is pushed onto the stack." num)))))
	(list
	 (cons word (funcall make-opcode num word))
	 (cons word-short (funcall make-opcode num word-short)))))
    ;; OP_PUSHBYTES are in range [1, 75]
    (number-sequence 1 16)))
  "List of `OP_{num}' and `OP_PUSHNUM_{num}' opcodes.")

(defconst bitcoin-script-mode--pushbytes-opcodes
  (mapcar
   (lambda (len)
     (let ((word (format "OP_PUSHBYTES_%d" len)))
       (cons
	word
	(make-btc-opcode
	 :word word
	 :code len
	 :input "None"
	 :output "bytes"
	 :description "Push value of bytes of data onto the stack."))))
   (number-sequence 1 75))
  "Return list of `OP_PUSHBYTES' opcodes `FROM' to `TO' lengths.")

(defconst bitcoin-script-mode--opcodes
  (append
   bitcoin-script-mode--pushnum-opcodes
   bitcoin-script-mode--pushbytes-opcodes
   (mapcar (lambda (l)
	     (let ((opcode (btc-opcode-from-list l)))
	       (cons (btc-opcode-word opcode) opcode)))
	   '(
	     ;; constants
	     ("OP_0" . (0 "None" "0" "An empty array of bytes is pushed onto the stack (false)."))
	     ("OP_FALSE" . (0 "None" "0" "Alias for OP_0."))
	     ("OP_TRUE" . (81 "None" "1" "Alias for OP_1."))

	     ;; Flow control
	     ("OP_NOP" . (97 "None" "None" "Does nothing."))
	     ("OP_IF" . (99 "x" "None" "If x is true, execute the following statements."))
	     ("OP_NOTIF" . (100 "x" "None" "If x is false, execute the following statements."))
	     ("OP_ELSE" . (103 "None" "None" "Executes the code after an OP_IF or OP_NOTIF if the preceding condition is false."))
	     ("OP_ENDIF" . (104 "None" "None" "Ends an if/else block."))
	     ("OP_VERIFY" . (105 "x" "None" "Marks transaction as invalid if x is not true."))
	     ("OP_RETURN" . (106 "None" "None" "Marks the transaction as invalid, can be used to store data in the blockchain."))

	     ;; Stack operations
	     ("OP_TOALTSTACK" . (107 "x" "None" "Moves x to the alt stack."))
	     ("OP_FROMALTSTACK" . (108 "None" "x" "Moves the top value from the alt stack to the main stack."))
	     ("OP_2DROP" . (109 "x1 x2" "None" "Removes the top two stack items."))
    	     ("OP_2DUP" . (110 "x1 x2" "x1, x2, x1, x2" "Duplicates the top two stack items."))
	     ("OP_3DUP" . (111 "x1 x2 x3" "x1, x2, x3, x1, x2, x3" "Duplicates the top three stack items."))
	     
	     ("OP_IFDUP" . (115 "x" "x, x (if x != 0)" "Duplicates x if it is not zero."))
	     ("OP_DEPTH" . (116 "None" "<stack size>" "Pushes the number of items on the stack."))
	     ("OP_DROP" . (117 "x" "None" "Removes the top item from the stack."))
	     ("OP_DUP" . (118 "x" "x, x" "Duplicates the top item on the stack."))
	     ("OP_NIP" . (119 "x1 x2" "x2" "Removes the second-to-top stack item."))
	     ("OP_OVER" . (120 "x1 x2" "x1 x2 x1" "Copies the second-to-top stack item to the top."))
	     ("OP_PICK" . (121 "xn n" "xn" "Copies the item n items back in the stack to the top."))
	     ("OP_ROLL" . (122 "xn n" "None" "Moves the item n items back in the stack to the top."))
	     ("OP_ROT" . (123 "x1 x2 x3" "x2 x3 x1" "Rotates the top three items on the stack."))
	     ("OP_SWAP" . (124 "x1 x2" "x2 x1" "Swaps the top two items on the stack."))
	     ("OP_TUCK" . (125 "x1 x2" "x2 x1 x2" "Copies the top item to below the second item."))

	     ;; Splice operations
	     ("OP_CAT" . (126 "x1 x2" "x1+x2" "Concatenates two strings."))
	     ("OP_SUBSTR" . (127 "x start length" "sub(x)" "Returns a substring of x starting at start with a length of length."))
	     ("OP_LEFT" . (128 "x size" "left part of x" "Keeps only characters left of the specified point in the string."))
	     ("OP_RIGHT" . (129 "x size" "right part of x" "Keeps only characters right of the specified point in the string."))
	     ("OP_SIZE" . (130 "x" "size" "Pushes the size of the top item on the stack."))

	     ;; Bitwise logic
	     ("OP_INVERT" . (131 "x" "~x" "Flips all the bits in the input."))
	     ("OP_AND" . (132 "x1 x2" "x1 & x2" "Boolean AND between each bit in the inputs."))
	     ("OP_OR" . (133 "x1 x2" "x1 | x2" "Boolean OR between each bit in the inputs."))
	     ("OP_XOR" . (134 "x1 x2" "x1 ^ x2" "Boolean XOR between each bit in the inputs."))
	     ("OP_EQUAL" . (135 "x1 x2" "True / False" "Returns 1 if the inputs are exactly equal, 0 otherwise."))
	     ("OP_EQUALVERIFY" . (136 "x1 x2" "None" "Same as OP_EQUAL, but runs OP_VERIFY afterward."))

	     ;; Arithmetic operations
	     ("OP_1ADD" . (139 "x" "x + 1" "Adds 1 to the input."))
	     ("OP_1SUB" . (140 "x" "x - 1" "Subtracts 1 from the input."))
	     ("OP_2MUL" . (141 "x" "2 * x" "Multiplies the input by 2."))
	     ("OP_2DIV" . (142 "x" "x / 2" "Divides the input by 2."))
	     ("OP_NEGATE" . (143 "x" "-x" "Negates the input."))
	     ("OP_ABS" . (144 "x" "abs(x)" "The input is made positive."))
	     ("OP_NOT" . (145 "x" "!x" "Returns 1 if the input is 0, 0 otherwise."))
	     ("OP_0NOTEQUAL" . (146 "x" "True / False" "Returns 0 if the input is 0, 1 otherwise."))
	     ("OP_ADD" . (147 "x1 x2" "out" "x1 is added to x2."))
	     ("OP_SUB" . (148 "x1 x2" "out" "x1 is subtracted to x2."))
	     ("OP_MUL" . (149 "x1 x2" "out" "x1 is multiplied to x2."))
	     ("OP_DIV" . (150 "x1 x2" "out" "x1 is divided to x2."))
	     ("OP_DIV" . (151 "x1 x2" "out" "x1 is divided to x2."))

	     ;; Crypto
	     ("OP_RIPEMD160" . (166 "x" "ripemd160(x)" "Hashes the input using RIPEMD-160."))
	     ("OP_SHA1" . (167 "x" "sha1(x)" "Hashes the input using SHA-1."))
	     ("OP_SHA256" . (168 "x" "sha256(x)" "Hashes the input using SHA-256."))
	     ("OP_HASH160" . (169 "x" "ripemd160(sha256(x))" "Hashes the input first with SHA-256 and then with RIPEMD-160."))
	     ("OP_HASH256" . (170 "x" "sha256(sha256(x))" "Hashes the input twice with SHA-256."))
	     ("OP_CODESEPARATOR" . (171 "None" "None" "All of the signature checking words will only match signatures to the data after the most recently-executed OP_CODESEPARATOR."))
	     ("OP_CHECKSIG" . (172 "sig pubkey" "True / False" "Verifies a signature against a public key."))
	     ("OP_CHECKSIGVERIFY" . (173 "sig pubkey" "None" "Same as OP_CHECKSIG, but runs OP_VERIFY afterward."))
	     ("OP_CHECKMULTISIG" . (174 "sig1 sig2... pubkey1 pubkey2..." "True / False" "Verifies multiple signatures against multiple public keys."))
	     ("OP_CHECKMULTISIGVERIFY" . (175 "sig1 sig2... pubkey1 pubkey2..." "None" "Same as OP_CHECKMULTISIG, but runs OP_VERIFY afterward."))
	     ("OP_CHECKSIGADD" . (186 "sig n pub" "out" "Three values are popped from the stack. The integer n is incremented by one and returned to the stack if the signature is valid for the public key and transaction. The integer n is returned to the stack unchanged if the signature is the empty vector (OP_0). In any other case, the script is invalid. This opcode is only available in tapscript."))
	     ("OP_CHECKLOCKTIMEVERIFY" . (177 "x" "x / fail" "Marks transaction as invalid if the top stack item is greater than the transaction's nLockTime field, otherwise script evaluation continues as though an OP_NOP was executed. Transaction is also invalid if 1. the stack is empty; or 2. the top stack item is negative; or 3. the top stack item is greater than or equal to 500000000 while the transaction's nLockTime field is less than 500000000, or vice versa; or 4. the input's nSequence field is equal to 0xffffffff. The precise semantics are described in BIP 0065."))
	     ("OP_CHECKSEQUENCEVERIFY" . (178 "x" "x / fail" "Marks transaction as invalid if the relative lock time of the input (enforced by BIP 0068 with nSequence) is not equal to or longer than the value of the top stack item. The precise semantics are described in BIP 0112.")))))
  "A list of Bitcoin Script opcodes with their input, output, and descriptions.")

(defconst bitcoin-script-mode--builtin-opcodes
  (mapcar (lambda (x) (car x)) bitcoin-script-mode--opcodes)
  "Bitcoin Script builtin OPCODES.")

(defun bitcoin-script-mode--re-keywords (keywords)
  "Produce a regexp matching some keywords of Bitcoin opcodes.
`KEYWORDS' a list of strings to match as Bitcoin Script opcodes."
  (concat "\\b" (regexp-opt keywords t) "\\b"))

(defvar bitcoin-script-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; C and C++-style comments.
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Lisp-style comments.
    (modify-syntax-entry ?\; "< b" table)
    table)
  "Syntax table for `ini-mode'.")

(defvar bitcoin-script-mode--font-lock-keywords
  `((,(bitcoin-script-mode--re-keywords bitcoin-script-mode--builtin-opcodes)
     (1 'font-lock-builtin-face))
    (,"\\([0-9a-fA-F]+\\)" (1 'font-lock-number-face)))
  "Highlight rules for `bitcoin-script-mode'.")

;;;###autoload
(define-derived-mode bitcoin-script-mode prog-mode "BtcScript"
  "A major mode for editing Bitcoin Script."
  (add-hook 'eldoc-documentation-functions #'bitcoin-script-mode-eldoc-function 0 t)
  (setq font-lock-defaults '(bitcoin-script-mode--font-lock-keywords nil)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.bs\\'" . bitcoin-script-mode))

(defun bitcoin-script-mode-format-region (start end)
  "Formats region from `START' to `END'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (while (not (eobp))
	(forward-whitespace 1)
	(let ((curr (char-after))
	      (prev (char-before)))
	  (cond
	   ;; if current whitespace is newline
	   ((eq curr ?\n)
	    ;; if previous is whitespace, remove it
	    ;;
	    ;; Like in case when "OP_CHECK \n", the space will be
	    ;; removed: "OP_CHECK\n"
	    (when (eq prev ?\s)
	      (backward-delete-char 1)))
	   ;; 
	   ((eq prev ?\n) nil)
	   ;; otherwise, delete whitespace and add newline
	   (t
	    (backward-delete-char 1)
	    (newline))))))))

(defun bitcoin-script-mode-format-buffer ()
  "Formats current buffer."
  (interactive)
  (let ((start (point-min))
	(end   (point-max)))
    (bitcoin-script-mode-format-region start end)))

(defun bitcoin-script-mode-eldoc-function (callback &rest _ignored)
  "Return a description for the Bitcoin Script opcode at point."
  (let ((opcode (thing-at-point 'symbol t)))
    (when opcode
      (let ((entry (assoc opcode bitcoin-script-mode--opcodes)))
	(when entry
	  (let ((op (cdr entry)))
	    (funcall callback
		     (bit-opcode-eldoc-string op)
		     :thing (btc-opcode-word op)
		     :face 'font-lock-builtin-face)))))))

(defun bitcoin-script-mode--company-backend (command &optional arg &rest ignored)
  "Company backend for bitcoin script mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-simple-backend))
    (prefix (and (eq major-mode 'bitcoin-script-mode)
                 (company-grab-symbol)))
    (candidates (cl-remove-if-not
		 (lambda (c) (string-prefix-p arg c))
                 (mapcar (lambda (x) x) bitcoin-script-mode--builtin-opcodes)))))

;;;###autoload

(provide 'bitcoin-script-mode)
;;; bitcoin-script-mode.el ends here
