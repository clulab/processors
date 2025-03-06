package org.clulab.odin.debugger.visualization

import scalatags.generic.Frag
import scalatags.text.Builder

object HtmlFragment {
  type Fragment = Frag[Builder, String]
}
