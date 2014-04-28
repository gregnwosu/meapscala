<map version="0.9.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node text="Scala">
<node text="syntax">
<node text="functional traits">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- interfaces for functions<br />- we introduce data types with [traits]</p></body>
</html>
</richcontent>
</node>
<node text="sealed trait">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- sealed means that all implentations must be in this file, kinda the same as [dinal]</p></body>
</html>
</richcontent>
</node>
</node>
</node>
<node text="other syntax quirks					      :drill:">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- each anonymous function has a implicit [apply] method that exposes the interface or [trait] of the function<br />- the function apply means that a function object can be called implicitly<br />- square brackets with capitals signify [generics]/[polymorphic] functions<br />- remember that anonymous functions should be wrapped in [brackets] and can name arguments only at [top] level<br />- note that applying 1 function after another requires repeated use of [brackets]</p><p>#+BEGIN_SRC scala<br />def uncurry[A,B,C](f: A =&gt; B=&gt; C): (A,B)=&gt;C =<br />(a:A,b:B) =&gt; f (a) (b)<br />#+END_SRC<br /></p></body>
</html>
</richcontent>
</node>
</node>
<node text="data constructors						      :drill:">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- varying forms use the case statement<br />&#160;&#160;- think overloading constructors<br />- are made by [extend]ing the trait<br />- each data constructor introduces a pattern</p></body>
</html>
</richcontent>
</node>
</node>
<node text="collections">
<node text="lists">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- lists can be made [polymorphic] with square brackets<br />- adding a + makes the list [covariant]<br />- Nil has a type [List[A]] for any A<br /></p></body>
</html>
</richcontent>
</node>
<node text="covariance">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- generally if X is a subtype of Y then <br />&#160;&#160;- List[X] is a subtupe of List[Y]</p><p></p><p></p></body>
</html>
</richcontent>
</node>
</node>
<node text="companion objects">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- has the same [name] as our data type<br />- also has [convience] functions</p></body>
</html>
</richcontent>
</node>
</node>
</node>
<node text="pattern matching">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>- _ matches any value<br />#+BEGIN_SRC scala</p><p>def sum(ints:List[Int]):Int = ints match {<br />case Nil =&gt; 0<br />case Cons(x,xs) =&gt; x + sum(xs)<br />}</p><p>#+END_SRC</p><p></p></body>
</html>
</richcontent>
</node>
<node text="syntatical quirks						      :drill:">
<node style="bubble" background_color="#eeee00">
<richcontent TYPE="NODE"><html>
<head>
<style type="text/css">
<!--
p { margin-top: 3px; margin-bottom: 3px; }-->
</style>
</head>
<body>
<p>SCHEDULED: &lt;2014-05-02 Fri&gt;</p><p>- need the ints for the match clause <br />-called the [target] or [scrutinee]<br />- case statements</p></body>
</html>
</richcontent>
<richcontent TYPE="NOTE"><html>
<head>
</head>
<body>
--org-mode:       :PROPERTIES:<br />
--org-mode:       :ID:       b186ff6c-d31c-45fc-b388-17a3ee768907<br />
--org-mode:       :DRILL_LAST_INTERVAL: 4.14<br />
--org-mode:       :DRILL_REPEATS_SINCE_FAIL: 2<br />
--org-mode:       :DRILL_TOTAL_REPEATS: 2<br />
--org-mode:       :DRILL_FAILURE_COUNT: 1<br />
--org-mode:       :DRILL_AVERAGE_QUALITY: 3.5<br />
--org-mode:       :DRILL_EASE: 2.6<br />
--org-mode:       :DRILL_LAST_QUALITY: 5<br />
--org-mode:       :DRILL_LAST_REVIEWED: [2014-04-28 Mon 10:54]<br />
--org-mode:       :END:<br />
</body>
</html>
</richcontent>
</node>
</node>
</node>
</node>
</node>
</node>
</map>
