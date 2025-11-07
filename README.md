# HLBCalc

**Hydrophilic-Lipophilic Balance Calculator**

A **fast, accurate, and extensible** HBL calculator written in **Common Lisp**.

---

## Features

- Calculate HLB from hydrophilic/lipophilic mass fractions  
- Classify surfactants by HLB range (W/O, O/W, solubilizer, etc.)  
- Compute **blended HLB** for surfactant mixtures  
- Built-in surfactant database (`:tween-80`, `:span-80`, etc.)  
- Clean, modern CL code
- Unit tests with **FiveAM**  
- ASDF + ocicl ready

---

## Installation

```bash
  # Clone the repo
  $ git clone https://github.com/logoraz/hlbcalc.git
  $ cd hlbcalc
  # Load in your Lisp REPL (SBCL, CCL, etc.)
  $ sbcl
```

```lisp
  ;; Load system
  (asdf:load-system :hlbcalc)

  ;; Test system
  (asdf:test-system :hlbcalc)

```


