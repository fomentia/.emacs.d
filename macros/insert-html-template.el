(fset 'insert-html-template
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("<html><head><title>q</title></head><body></body></html>" 0 "%d")) arg)))
