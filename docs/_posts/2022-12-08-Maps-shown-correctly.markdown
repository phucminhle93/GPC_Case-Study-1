---
layout: post
title:  "Interactive Maps worked!"
date:   2022-12-08 13:05:00 +0100
categories: Site Update
---

The interactive maps in Visualization 1 finally displays correctly and can be played with! The previous display failure was due to a build warning when Github was (re)building the website following push:

```
Liquid Warning: Liquid syntax error (line 470): Unexpected character & in "{{if(t&&"string"==typeof t)return u.hasOwnProperty(t)||(u[t]=new a(t)),u[t];if("object"===(void 0===t?"undefined":r(t))&&t._vars&&t.var)return t;if(Array.isArray(t)&&1==t.length&&"string"==typeof t[0])return e(t[0]);throw new Error("Invalid groupName argument")}}" in Viz-Maps.html
```

The warning shows that there is this unexpected character "&" in a line in a certain .html file. This file happens to be the one that displays the interactive maps.

To fix this:

    1. Go to the .html file where the warning occurs (in this case, it's Viz-Map.html which has the interactive maps) 

    2. Search for the code line that gave the warning 

    3. Add a space between "\{\{" and "\}\}" so they become "{ {" and "} }" 

    4. Save the file and update the changes (git add, git commit, git push) 
