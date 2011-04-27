require 'irb/completion'
require 'wirble'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:USE_READLINE] = true

Wirble.init
Wirble.colorize
