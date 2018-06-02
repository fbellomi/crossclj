/**
 * SyntaxHighlighter
 * http://alexgorbatchev.com/SyntaxHighlighter
 *
 * SyntaxHighlighter is donationware. If you are using it, please donate.
 * http://alexgorbatchev.com/SyntaxHighlighter/donate.html
 *
 * @version
 * 3.0.83 (Sat, 28 Jun 2014 09:04:06 GMT)
 *
 * @copyright
 * Copyright (C) 2004-2013 Alex Gorbatchev.
 *
 * @license
 * Dual licensed under the MIT and GPL licenses.
 */
var XRegExp;if(XRegExp=XRegExp||function(e){"use strict";function t(e,t,n){var r;for(r in c.prototype)c.prototype.hasOwnProperty(r)&&(e[r]=c.prototype[r]);return e.xregexp={captureNames:t,isNative:!!n},e}function n(e){return(e.global?"g":"")+(e.ignoreCase?"i":"")+(e.multiline?"m":"")+(e.extended?"x":"")+(e.sticky?"y":"")}function r(e,r,i){if(!c.isRegExp(e))throw new TypeError("type RegExp expected");var a=p.replace.call(n(e)+(r||""),w,"");return i&&(a=p.replace.call(a,RegExp("["+i+"]+","g"),"")),e=e.xregexp&&!e.xregexp.isNative?t(c(e.source,a),e.xregexp.captureNames?e.xregexp.captureNames.slice(0):null):t(RegExp(e.source,a),null,!0)}function i(e,t){var n=e.length;if(Array.prototype.lastIndexOf)return e.lastIndexOf(t);for(;n--;)if(e[n]===t)return n;return-1}function a(e,t){return Object.prototype.toString.call(e).toLowerCase()==="[object "+t+"]"}function l(e){return e=e||{},"all"===e||e.all?e={natives:!0,extensibility:!0}:a(e,"string")&&(e=c.forEach(e,/[^\s,]+/,function(e){this[e]=!0},{})),e}function s(e,t,n,r){var i,a,l=m.length,s=null;H=!0;try{for(;l--;)if(a=m[l],!("all"!==a.scope&&a.scope!==n||a.trigger&&!a.trigger.call(r))&&(a.pattern.lastIndex=t,i=d.exec.call(a.pattern,e),i&&i.index===t)){s={output:a.handler.call(r,i,n),match:i};break}}catch(u){throw u}finally{H=!1}return s}function u(e){c.addToken=g[e?"on":"off"],f.extensibility=e}function o(e){RegExp.prototype.exec=(e?d:p).exec,RegExp.prototype.test=(e?d:p).test,String.prototype.match=(e?d:p).match,String.prototype.replace=(e?d:p).replace,String.prototype.split=(e?d:p).split,f.natives=e}var c,g,h,f={natives:!1,extensibility:!1},p={exec:RegExp.prototype.exec,test:RegExp.prototype.test,match:String.prototype.match,replace:String.prototype.replace,split:String.prototype.split},d={},x={},m=[],v="default",y="class",b={"default":/^(?:\\(?:0(?:[0-3][0-7]{0,2}|[4-7][0-7]?)?|[1-9]\d*|x[\dA-Fa-f]{2}|u[\dA-Fa-f]{4}|c[A-Za-z]|[\s\S])|\(\?[:=!]|[?*+]\?|{\d+(?:,\d*)?}\??)/,"class":/^(?:\\(?:[0-3][0-7]{0,2}|[4-7][0-7]?|x[\dA-Fa-f]{2}|u[\dA-Fa-f]{4}|c[A-Za-z]|[\s\S]))/},E=/\$(?:{([\w$]+)}|(\d\d?|[\s\S]))/g,w=/([\s\S])(?=[\s\S]*\1)/g,N=/^(?:[?*+]|{\d+(?:,\d*)?})\??/,S=p.exec.call(/()??/,"")[1]===e,R=RegExp.prototype.sticky!==e,H=!1,L="gim"+(R?"y":"");return c=function(n,i){if(c.isRegExp(n)){if(i!==e)throw new TypeError("can't supply flags when constructing one RegExp from another");return r(n)}if(H)throw Error("can't call the XRegExp constructor within token definition functions");var a,l,u,o=[],g=v,h={hasNamedCapture:!1,captureNames:[],hasFlag:function(e){return i.indexOf(e)>-1}},f=0;if(n=n===e?"":n+"",i=i===e?"":i+"",p.match.call(i,w))throw new SyntaxError("invalid duplicate regular expression flag");for(n=p.replace.call(n,/^\(\?([\w$]+)\)/,function(e,t){if(p.test.call(/[gy]/,t))throw new SyntaxError("can't use flag g or y in mode modifier");return i=p.replace.call(i+t,w,""),""}),c.forEach(i,/[\s\S]/,function(e){if(0>L.indexOf(e[0]))throw new SyntaxError("invalid regular expression flag "+e[0])});n.length>f;)a=s(n,f,g,h),a?(o.push(a.output),f+=a.match[0].length||1):(l=p.exec.call(b[g],n.slice(f)),l?(o.push(l[0]),f+=l[0].length):(u=n.charAt(f),"["===u?g=y:"]"===u&&(g=v),o.push(u),++f));return t(RegExp(o.join(""),p.replace.call(i,/[^gimy]+/g,"")),h.hasNamedCapture?h.captureNames:null)},g={on:function(e,t,n){n=n||{},e&&m.push({pattern:r(e,"g"+(R?"y":"")),handler:t,scope:n.scope||v,trigger:n.trigger||null}),n.customFlags&&(L=p.replace.call(L+n.customFlags,w,""))},off:function(){throw Error("extensibility must be installed before using addToken")}},c.addToken=g.off,c.cache=function(e,t){var n=e+"/"+(t||"");return x[n]||(x[n]=c(e,t))},c.escape=function(e){return p.replace.call(e,/[-[\]{}()*+?.,\\^$|#\s]/g,"\\$&")},c.exec=function(e,t,n,i){var a,l=r(t,"g"+(i&&R?"y":""),i===!1?"y":"");return l.lastIndex=n=n||0,a=d.exec.call(l,e),i&&a&&a.index!==n&&(a=null),t.global&&(t.lastIndex=a?l.lastIndex:0),a},c.forEach=function(e,t,n,r){for(var i,a=0,l=-1;i=c.exec(e,t,a);)n.call(r,i,++l,e,t),a=i.index+(i[0].length||1);return r},c.globalize=function(e){return r(e,"g")},c.install=function(e){e=l(e),!f.natives&&e.natives&&o(!0),!f.extensibility&&e.extensibility&&u(!0)},c.isInstalled=function(e){return!!f[e]},c.isRegExp=function(e){return a(e,"regexp")},c.matchChain=function(e,t){return function n(e,r){var i,a=t[r].regex?t[r]:{regex:t[r]},l=[],s=function(e){l.push(a.backref?e[a.backref]||"":e[0])};for(i=0;e.length>i;++i)c.forEach(e[i],a.regex,s);return r!==t.length-1&&l.length?n(l,r+1):l}([e],0)},c.replace=function(t,n,i,a){var l,s=c.isRegExp(n),u=n;return s?(a===e&&n.global&&(a="all"),u=r(n,"all"===a?"g":"","all"===a?"":"g")):"all"===a&&(u=RegExp(c.escape(n+""),"g")),l=d.replace.call(t+"",u,i),s&&n.global&&(n.lastIndex=0),l},c.split=function(e,t,n){return d.split.call(e,t,n)},c.test=function(e,t,n,r){return!!c.exec(e,t,n,r)},c.uninstall=function(e){e=l(e),f.natives&&e.natives&&o(!1),f.extensibility&&e.extensibility&&u(!1)},c.union=function(e,t){var n,r,i,l,s=/(\()(?!\?)|\\([1-9]\d*)|\\[\s\S]|\[(?:[^\\\]]|\\[\s\S])*]/g,u=0,o=function(e,t,i){var a=r[u-n];if(t){if(++u,a)return"(?<"+a+">"}else if(i)return"\\"+(+i+n);return e},g=[];if(!a(e,"array")||!e.length)throw new TypeError("patterns must be a nonempty array");for(l=0;e.length>l;++l)i=e[l],c.isRegExp(i)?(n=u,r=i.xregexp&&i.xregexp.captureNames||[],g.push(c(i.source).source.replace(s,o))):g.push(c.escape(i));return c(g.join("|"),t)},c.version="2.0.0",d.exec=function(t){var r,a,l,s,u;if(this.global||(s=this.lastIndex),r=p.exec.apply(this,arguments)){if(!S&&r.length>1&&i(r,"")>-1&&(l=RegExp(this.source,p.replace.call(n(this),"g","")),p.replace.call((t+"").slice(r.index),l,function(){var t;for(t=1;arguments.length-2>t;++t)arguments[t]===e&&(r[t]=e)})),this.xregexp&&this.xregexp.captureNames)for(u=1;r.length>u;++u)a=this.xregexp.captureNames[u-1],a&&(r[a]=r[u]);this.global&&!r[0].length&&this.lastIndex>r.index&&(this.lastIndex=r.index)}return this.global||(this.lastIndex=s),r},d.test=function(e){return!!d.exec.call(this,e)},d.match=function(e){if(c.isRegExp(e)){if(e.global){var t=p.match.apply(this,arguments);return e.lastIndex=0,t}}else e=RegExp(e);return d.exec.call(e,this)},d.replace=function(e,t){var n,r,l,s,u=c.isRegExp(e);return u?(e.xregexp&&(n=e.xregexp.captureNames),e.global||(s=e.lastIndex)):e+="",a(t,"function")?r=p.replace.call(this+"",e,function(){var r,i=arguments;if(n)for(i[0]=new String(i[0]),r=0;n.length>r;++r)n[r]&&(i[0][n[r]]=i[r+1]);return u&&e.global&&(e.lastIndex=i[i.length-2]+i[0].length),t.apply(null,i)}):(l=this+"",r=p.replace.call(l,e,function(){var e=arguments;return p.replace.call(t+"",E,function(t,r,a){var l;if(r){if(l=+r,e.length-3>=l)return e[l]||"";if(l=n?i(n,r):-1,0>l)throw new SyntaxError("backreference to undefined group "+t);return e[l+1]||""}if("$"===a)return"$";if("&"===a||0===+a)return e[0];if("`"===a)return e[e.length-1].slice(0,e[e.length-2]);if("'"===a)return e[e.length-1].slice(e[e.length-2]+e[0].length);if(a=+a,!isNaN(a)){if(a>e.length-3)throw new SyntaxError("backreference to undefined group "+t);return e[a]||""}throw new SyntaxError("invalid token "+t)})})),u&&(e.lastIndex=e.global?0:s),r},d.split=function(t,n){if(!c.isRegExp(t))return p.split.apply(this,arguments);var r,i=this+"",a=t.lastIndex,l=[],s=0;return n=(n===e?-1:n)>>>0,c.forEach(i,t,function(e){e.index+e[0].length>s&&(l.push(i.slice(s,e.index)),e.length>1&&e.index<i.length&&Array.prototype.push.apply(l,e.slice(1)),r=e[0].length,s=e.index+r)}),s===i.length?(!p.test.call(t,"")||r)&&l.push(""):l.push(i.slice(s)),t.lastIndex=a,l.length>n?l.slice(0,n):l},h=g.on,h(/\\([ABCE-RTUVXYZaeg-mopqyz]|c(?![A-Za-z])|u(?![\dA-Fa-f]{4})|x(?![\dA-Fa-f]{2}))/,function(e,t){if("B"===e[1]&&t===v)return e[0];throw new SyntaxError("invalid escape "+e[0])},{scope:"all"}),h(/\[(\^?)]/,function(e){return e[1]?"[\\s\\S]":"\\b\\B"}),h(/(?:\(\?#[^)]*\))+/,function(e){return p.test.call(N,e.input.slice(e.index+e[0].length))?"":"(?:)"}),h(/\\k<([\w$]+)>/,function(e){var t=isNaN(e[1])?i(this.captureNames,e[1])+1:+e[1],n=e.index+e[0].length;if(!t||t>this.captureNames.length)throw new SyntaxError("backreference to undefined group "+e[0]);return"\\"+t+(n===e.input.length||isNaN(e.input.charAt(n))?"":"(?:)")}),h(/(?:\s+|#.*)+/,function(e){return p.test.call(N,e.input.slice(e.index+e[0].length))?"":"(?:)"},{trigger:function(){return this.hasFlag("x")},customFlags:"x"}),h(/\./,function(){return"[\\s\\S]"},{trigger:function(){return this.hasFlag("s")},customFlags:"s"}),h(/\(\?P?<([\w$]+)>/,function(e){if(!isNaN(e[1]))throw new SyntaxError("can't use integer as capture name "+e[0]);return this.captureNames.push(e[1]),this.hasNamedCapture=!0,"("}),h(/\\(\d+)/,function(e,t){if(!(t===v&&/^[1-9]/.test(e[1])&&+e[1]<=this.captureNames.length)&&"0"!==e[1])throw new SyntaxError("can't use octal escape or backreference to undefined group "+e[0]);return e[0]},{scope:"all"}),h(/\((?!\?)/,function(){return this.hasFlag("n")?"(?:":(this.captureNames.push(null),"(")},{customFlags:"n"}),"undefined"!=typeof exports&&(exports.XRegExp=c),c}(),SyntaxHighlighter===void 0)var SyntaxHighlighter=function(){function e(e,t){return-1!=e.className.indexOf(t)}function t(t,n){e(t,n)||(t.className+=" "+n)}function n(e,t){e.className=e.className.replace(t,"")}function r(e){for(var t=[],n=0,r=e.length;r>n;n++)t.push(e[n]);return t}function i(e){return e.split(/\r?\n/)}function a(e){var t="highlighter_";return 0==e.indexOf(t)?e:t+e}function l(e){return H.vars.highlighters[a(e)]}function s(e){H.vars.highlighters[a(e.id)]=e}function u(e,t,n){if(null==e)return null;var r,i,a=1!=n?e.childNodes:[e.parentNode],l={"#":"id",".":"className"}[t.substr(0,1)]||"nodeName";if(r="nodeName"!=l?t.substr(1):t.toUpperCase(),-1!=(e[l]||"").indexOf(r))return e;for(var s=0,o=a.length;a&&o>s&&null==i;s++)i=u(a[s],t,n);return i}function o(e,t){return u(e,t,!0)}function c(e,t,n){n=Math.max(n||0,0);for(var r=n,i=e.length;i>r;r++)if(e[r]==t)return r;return-1}function g(e){return(e||"")+(""+Math.round(1e6*Math.random()))}function h(e,t){var n,r={};for(n in e)r[n]=e[n];for(n in t)r[n]=t[n];return r}function f(e){var t={"true":!0,"false":!1}[e];return null==t?e:t}function p(e,t,n,r){function i(e){e=e||window.event,e.target||(e.target=e.srcElement,e.preventDefault=function(){this.returnValue=!1}),n.call(r||window,e)}e.attachEvent?e.attachEvent("on"+t,i):e.addEventListener(t,i,!1)}function d(e,t){var n=H.vars.discoveredBrushes,r=null;if(null==n){n={};for(var i in H.brushes){var a=H.brushes[i],l=a.aliases;if(null!=l){a.brushName=i.toLowerCase();for(var s=0,u=l.length;u>s;s++)n[l[s]]=i}}H.vars.discoveredBrushes=n}return r=H.brushes[n[e]],null==r&&t&&alert(H.config.strings.noBrush+e),r}function x(e,t){for(var n=i(e),r=0,a=n.length;a>r;r++)n[r]=t(n[r],r);return n.join("\n")}function m(e){return e.replace(/^[ ]*[\n]+|[\n]*[ ]*$/g,"")}function v(e){for(var t,n={},r=XRegExp("^\\[(?<values>(.*?))\\]$"),i=0,a=XRegExp("(?<name>[\\w-]+)\\s*:\\s*(?<value>[\\w%#-]+|\\[.*?\\]|\".*?\"|'.*?')\\s*;?","g");null!=(t=XRegExp.exec(e,a,i));){var l=t.value.replace(/^['"]|['"]$/g,"");if(null!=l&&r.test(l)){var s=XRegExp.exec(l,r);l=s.values.length>0?s.values.split(/\s*,\s*/):[]}n[t.name]=l,i=t.index+t[0].length}return n}function y(e,t){return null==e||0==e.length||"\n"==e?e:(e=e.replace(/</g,"&lt;"),null!=t&&(e=x(e,function(e){if(0==e.length)return"";var n="";return e=e.replace(/^( )+/,function(e){return n=e,""}),0==e.length?n:n+'<code class="'+t+'">'+e+"</code>"})),e)}function b(e,t){for(var n=""+e;t>n.length;)n="0"+n;return n}function E(e){return e.replace(/^\s+|\s+$/g,"")}function w(e,t){return e.index<t.index?-1:e.index>t.index?1:e.length<t.length?-1:e.length>t.length?1:0}function N(e,t){function n(e){return e[0]}var r=null,i=[],a=t.func?t.func:n;for(pos=0;null!=(r=XRegExp.exec(e,t.regex,pos));){var l=a(r,t);"string"==typeof l&&(l=[new H.Match(l,r.index,t.css)]),i=i.concat(l),pos=r.index+r[0].length}return i}function S(e){return e.replace(/"ç@(.)([^"ç@]+)ç@([^"ç@]+)"/g,'<a class="$1" href="$3">$2</a>').replace(/"çç@([^"ç@]+)ç@([^"ç@]+)"/g,'<a target="_blank" href="http://$2">$1</a>').replace(/"ç@([^"ç@]+)"/g,'<a id="$1"></a>')}function R(e){var r,i=e.target,a=o(i,".syntaxhighlighter"),s=o(i,".container"),c=document.createElement("textarea");if(s&&a&&!u(s,"textarea")){r=l(a.id),t(a,"source");for(var g=s.childNodes,h=[],f=0,d=g.length;d>f;f++)h.push(g[f].innerText||g[f].textContent);h=h.join("\r"),h=h.replace(/\u00a0/g," "),c.appendChild(document.createTextNode(h)),s.appendChild(c),c.focus(),c.select(),p(c,"blur",function(){c.parentNode.removeChild(c),n(a,"source")})}}"undefined"!=typeof require&&XRegExp===void 0&&(XRegExp=require("xregexp").XRegExp);var H={defaults:{"class-name":"","first-line":1,"pad-line-numbers":!1,highlight:null,title:null,"smart-tabs":!0,"tab-size":4,toolbar:!1,"quick-code":!0,collapse:!1,"auto-links":!1,light:!1,unindent:!1,"html-script":!1},config:{space:" ",useScriptTags:!1,bloggerMode:!1,stripBrs:!1,tagName:"pre",strings:{expandSource:"expand source",help:"?",alert:"SyntaxHighlighter\n\n",noBrush:"Can't find brush for: ",brushNotHtmlScript:"Brush wasn't configured for html-script option: ",aboutDialog:""}},vars:{discoveredBrushes:null,highlighters:{}},brushes:{},regexLib:{multiLineCComments:XRegExp("/\\*.*?\\*/","gs"),singleLineCComments:/\/\/.*$/gm,singleLinePerlComments:/#.*$/gm,doubleQuotedString:/"([^\\"\n]|\\.)*"/g,singleQuotedString:/'([^\\'\n]|\\.)*'/g,multiLineDoubleQuotedString:XRegExp('"([^\\\\"]|\\\\.)*"',"gs"),multiLineSingleQuotedString:XRegExp("'([^\\\\']|\\\\.)*'","gs"),xmlComments:XRegExp("(&lt;|<)!--.*?--(&gt;|>)","gs"),url:/\w+:\/\/[\w-.\/?%&=:@;#]*/g,phpScriptTags:{left:/(&lt;|<)\?(?:=|php)?/g,right:/\?(&gt;|>)/g,eof:!0},aspScriptTags:{left:/(&lt;|<)%=?/g,right:/%(&gt;|>)/g},scriptScriptTags:{left:/(&lt;|<)\s*script.*?(&gt;|>)/gi,right:/(&lt;|<)\/\s*script\s*(&gt;|>)/gi}},findElements:function(e,t){var n=t?[t]:r(document.getElementsByTagName(H.config.tagName)),i=(H.config,[]);if(0===n.length)return i;for(var a=0,l=n.length;l>a;a++){var s={target:n[a],params:h(e,v(n[a].className))};null!=s.params.brush&&i.push(s)}return i},highlight:function(e,t){var n=this.findElements(e,t),r="innerHTML",i=null;if(H.config,0!==n.length)for(var a=0,l=n.length;l>a;a++){var s,t=n[a],u=t.target,o=t.params,c=o.brush;if(null!=c){var g=d(c);g&&(i=new g,s=u[r],""!=(u.title||"")&&(o.title=u.title),o.brush=c,i.init(o),t=i.getDiv(s),""!=(u.id||"")&&(t.id=u.id),u.parentNode.replaceChild(t,u))}}},all:function(e){$(function(){H.highlight(e),document.getElementById("cont1").setAttribute("style","display:auto"),window.location.hash.length>1&&(window.location.href=window.location.hash)})}};return H.Match=function(e,t,n){this.value=e,this.index=t,this.length=e.length,this.css=n,this.brushName=null},H.Match.prototype.toString=function(){return this.value},H.HtmlScript=function(e){function t(e,t){for(var n=0,r=e.length;r>n;n++)e[n].index+=t}function n(e){for(var n,a=e.code,l=[],s=r.regexList,u=e.index+e.left.length,o=r.htmlScript,c=0,g=s.length;g>c;c++)n=N(a,s[c]),t(n,u),l=l.concat(n);null!=o.left&&null!=e.left&&(n=N(e.left,o.left),t(n,e.index),l=l.concat(n)),null!=o.right&&null!=e.right&&(n=N(e.right,o.right),t(n,e.index+e[0].lastIndexOf(e.right)),l=l.concat(n));for(var h=0,g=l.length;g>h;h++)l[h].brushName=i.brushName;return l}var r,i=d(e),a=new H.brushes.Xml,l=this,s="getDiv getHtml init".split(" ");if(null!=i){r=new i;for(var u=0,o=s.length;o>u;u++)(function(){var e=s[u];l[e]=function(){return a[e].apply(a,arguments)}})();a.regexList.push({regex:r.htmlScript.code,func:n})}},H.Highlighter=function(){},H.Highlighter.prototype={getParam:function(e,t){var n=this.params[e];return f(null==n?t:n)},create:function(e){return document.createElement(e)},findMatches:function(e,t){var n=[];if(null!=e)for(var r=0,i=e.length;i>r;r++)"object"==typeof e[r]&&(n=n.concat(N(t,e[r])));return this.removeNestedMatches(n.sort(w))},removeNestedMatches:function(e){for(var t=0,n=e.length;n>t;t++)if(null!==e[t])for(var r=e[t],i=r.index+r.length,a=t+1,n=e.length;n>a&&null!==e[t];a++){var l=e[a];if(null!==l){if(l.index>i)break;l.index==r.index&&l.length>r.length?e[t]=null:l.index>=r.index&&i>l.index&&(e[a]=null)}}return e},figureOutLineNumbers:function(e){var t=[],n=parseInt(this.getParam("first-line"));return x(e,function(e,r){t.push(r+n)}),t},isLineHighlighted:function(e){var t=this.getParam("highlight",[]);return"object"!=typeof t&&null==t.push&&(t=[t]),-1!=c(t,""+e)},getLineHtml:function(e,t,n){var r=["line","nu"+t];return this.isLineHighlighted(t)&&r.push("highlighted"),0==t&&r.push("break"),'<div class="'+r.join(" ")+'">'+n+"</div>"},getLineNumbersHtml:function(e,t){var n="",r=i(e).length,a=parseInt(this.getParam("first-line")),l=this.getParam("pad-line-numbers");1==l?l=(""+(a+r-1)).length:1==isNaN(l)&&(l=0);for(var s=0;r>s;s++){var u=t?t[s]:a+s,e=0==u?H.config.space:b(u,l);n+=this.getLineHtml(s,u,e)}return n},getCodeLinesHtml:function(e,t){e=E(e);var n=i(e),r=(this.getParam("pad-line-numbers"),parseInt(this.getParam("first-line"))),e="";this.getParam("brush");for(var a=0,l=n.length;l>a;a++){var s=n[a],u=t?t[a]:r+a;0==s.length&&(s=H.config.space),e+=this.getLineHtml(a,u,s)}return e},getTitleHtml:function(e){return e?"<caption>"+e+"</caption>":""},getMatchesHtml:function(e,t){function n(e){var t=e?e.brushName||a:a;return t?t+" ":""}for(var r=0,i="",a=this.getParam("brush",""),l=0,s=t.length;s>l;l++){var u,o=t[l];null!==o&&0!==o.length&&(u=n(o),i+=y(e.substr(r,o.index-r),u+"plain")+y(o.value,u+o.css),r=o.index+o.length+(o.offset||0))}return i+=y(e.substr(r),n()+"plain")},getHtml:function(e){var t,n,r="",i=["syntaxhighlighter"];return className="syntaxhighlighter",i.push(this.getParam("brush")),e=m(e).replace(/\r/g," "),n=this.figureOutLineNumbers(e),t=this.findMatches(this.regexList,e),r=this.getMatchesHtml(e,t),r=this.getCodeLinesHtml(r,n),r=S(r),"undefined"!=typeof navigator&&navigator.userAgent&&navigator.userAgent.match(/MSIE/)&&i.push("ie"),r='<div id="'+a(this.id)+'" class="'+i.join(" ")+'">'+'<table border="0" cellpadding="0" cellspacing="0">'+this.getTitleHtml(this.getParam("title"))+"<tbody>"+"<tr>"+'<td class="gutter">'+this.getLineNumbersHtml(e)+"</td>"+'<td class="code">'+'<div class="container">'+r+"</div>"+"</td>"+"</tr>"+"</tbody>"+"</table>"+"</div>"},getDiv:function(e){null===e&&(e=""),this.code=e;var t=this.create("div");return t.innerHTML=this.getHtml(e),this.getParam("quick-code")&&p(u(t,".code"),"dblclick",R),t},init:function(e){this.id=g(),s(this),this.params=h(H.defaults,e||{})},getKeywords:function(e){return e=e.replace(/^\s+|\s+$/g,"").replace(/\s+/g,"|"),"\\b(?:"+e+")\\b"},forHtmlScript:function(e){var t={end:e.right.source};e.eof&&(t.end="(?:(?:"+t.end+")|$)"),this.htmlScript={left:{regex:e.left,css:"script"},right:{regex:e.right,css:"script"},code:XRegExp("(?<left>"+e.left.source+")"+"(?<code>.*?)"+"(?<right>"+t.end+")","sgi")}}},H}();"undefined"!=typeof exports?exports.SyntaxHighlighter=SyntaxHighlighter:null;// (inc clojure-brush) ;; an improved SyntaxHighlighter brush for clojure
//
// Copyright (C) 2011 Andrew Brehaut
//
// Distributed under the Eclipse Public License, the same as Clojure.
//
// https://github.com/brehaut/inc-clojure-brush
//
// Written by Andrew Brehaut
// V0.9.1, November 2011

if (typeof net == "undefined") net = {};
if (!(net.brehaut)) net.brehaut = {};

net.brehaut.ClojureTools = (function (SH) {
  "use strict";
  // utiliies
  if (!Object.create) Object.create = function object(o) {
    function F() {};
    F.prototype = o;
    return new F();
  };

  // data

  function Token(value, index, tag, length) {
    this.value = value;
    this.index = index;
    this.length = length || value.length;
    this.tag = tag;
    this.secondary_tags = {};
  }

  // null_token exists so that LispNodes that have not had a closing tag attached
  // can have a dummy token to simplify annotation
  var null_token = new Token("", -1, "null", -1);

  /* LispNodes are aggregate nodes for sexpressions.
   *
   */
  function LispNode(tag, children, opening) {
    this.tag = tag;            // current metadata for syntax inference
    this.parent = null;        // the parent expression
    this.list = children;      // all the child forms in order
    this.opening = opening;    // the token that opens this form.
    this.closing = null_token; // the token that closes this form.
    this.meta = null;          // metadata nodes will be attached here if they are found
  }

  var null_lispnode = new LispNode("null", [], null_token);


  function PrefixNode(tag, token, attached_node) {
    this.tag = tag;
    this.token = token;
    this.attached_node = attached_node;
    this.parent = null;
  }



  // tokenize

  function tokenize(code) {
    var tokens = [];
    var tn = 0;

    var zero = "0".charCodeAt(0);
    var nine = "9".charCodeAt(0);
    var lower_a = "a".charCodeAt(0);
    var lower_f = "f".charCodeAt(0);
    var upper_a = "A".charCodeAt(0);
    var upper_f = "F".charCodeAt(0);

    var dispatch = false; // have we just seen a # character?

    // i tracks the start of the current window
    // extent is the window for slicing

    for (var i = 0,
             extent = i,
             j = code.length;
             i < j && extent <= j;) {

      var c = code[i];

      // we care about capturing the whole token when dispatch is used, so back up the
      // starting index by 1
      if (dispatch) i--;

      switch (c) {
        // dispatch alters the value of the next thing read
        case "#":
          dispatch = true;
          i++;
          extent++;
          continue;

        case " ":    // ignore whitespace
        case "\t":
        case "\n":
        case "\r":
        case ",":
          extent++
          break;

        // simple terms
        case "^":
        case "`":
        case ")":
        case "[":
        case "]":
        case "}":
        case "@":
          tokens[tn++] = new Token(c, i, c, ++extent - i);
          break;

        case "'":
          tokens[tn++] = new Token(code.slice(i, ++extent), i, dispatch ? "#'" : "'", extent - i);
          break

        case "(":
          tokens[tn++] = new Token(code.slice(i, ++extent), i, dispatch ? "#(" : "(", extent - i);
          break;

        case "{":
          tokens[tn++] = new Token(code.slice(i, ++extent), i, dispatch ? "#{" : "{", extent - i);
          break;

        case "\\":
          if (code.slice(i + 1, i + 8) === "newline") {
            tokens[tn++] = new Token("\\newline", i, "value", 8);
            extent = i + 9;
          }
          else if (code.slice(i + 1, i + 6) === "space") {
            tokens[tn++] = new Token("\\space", i, "value", 6);
            extent = i + 6;
          }
          else if (code.slice(i + 1, i + 4) === "tab") {
            tokens[tn++] = new Token("\\tab", i, "value", 4);
            extent = i + 5;
          } // work around fun bug with &,>,< in character literals
          else if (code.slice(i + 1, i + 6) === "&amp;") {
            tokens[tn++] = new Token("\\&amp;", i, "value", 6);
            extent = i + 6;
          }
          else if (code.slice(i + 1, i + 5) === "&lt;") {
            tokens[tn++] = new Token("\\&lt;", i, "value", 5);
            extent = i + 5;
          }
          else if (code.slice(i + 1, i + 5) === "&gt;") {
            tokens[tn++] = new Token("\\&gt;", i, "value", 5);
            extent = i + 5;
          }

          else {
            extent += 2;
            tokens[tn++] = new Token(code.slice(i, extent), i, "value", 2);
          }
          break;

        case "~": // slice
          if (code[i + 1] === "@") {
            extent += 2;
            tokens[tn++] = new Token(code.slice(i, extent), i, "splice", 2);
          }
          else {
            tokens[tn++] = new Token(code.slice(i, ++extent), i, "unquote", 2);
          }
          break;

        // complicated terms
        case "\"": // strings and regexps
          for (extent++; extent <= j; extent++) {
            if (code[extent] === "\\") extent++;
            else if (code[extent] === "\"") break;
          }
          tokens[tn++] = new Token(code.slice(i, ++extent), i, dispatch ? "regexp" : "string", extent - i);
          break;

        case ";":
          for (; extent <= j && code[extent] !== "\n" && code[extent] !== "\r"; extent++);
          tokens[tn++] = new Token(code.slice(i, ++extent), i, "comments", extent - i);
          break;

        case "+": // numbers; fall through to symbol for + and - not prefixing a number
        case "-":
        case "0":
        case "1":
        case "2":
        case "3":
        case "4":
        case "5":
        case "6":
        case "7":
        case "8":
        case "9":
        // todo: exponents, hex
        // http://my.safaribooksonline.com/9781449310387/14?reader=pf&readerfullscreen=&readerleftmenu=1
          var c2 = code.charCodeAt(i + 1);
          if (((c === "+" || c === "-") && (c2 >= zero && c2 <= nine)) // prefixes
              || (c !== "+" && c !== "-")) {
            if (c === "+" || c === "-") extent++;
            for (; extent <= j; extent++) {
              var charCode = code.charCodeAt(extent);
              if (charCode < zero || charCode > nine) break;
            }

            c = code[extent];
            c2 = code.charCodeAt(extent + 1);
            if ((c === "r" || c === "R" || c === "/" || c === ".") // interstitial characters
                && (c2 >= zero && c2 <= nine)) {
              for (extent++; extent <= j; extent++) {
                var charCode = code.charCodeAt(extent);
                if (charCode < zero || charCode > nine) break;
              }
            }

            c = code[extent];
            c2 = code.charCodeAt(extent + 1);
            if ((c === "x" || c === "X") &&
                ((c2 >= zero && c2 <= nine)
                 || (c2 >= lower_a && c2 <= lower_f)
                 || (c2 >= upper_a && c2 <= upper_f))) {
              for (extent++; extent <= j; extent++) {
                var charCode = code.charCodeAt(extent);
                if (((charCode >= zero && charCode <= nine)
                    || (charCode >= lower_a && charCode <= lower_f)
                    || (charCode >= upper_a && charCode <= upper_f))) continue;
                break;
              }
            }

            c = code[extent];
            c2 = code.charCodeAt(extent + 1);
            if ((c === "e" || c === "E")
                && (c2 >= zero && c2 <= nine)) {
              for (extent++; extent <= j; extent++) {
                var charCode = code.charCodeAt(extent);
                if (charCode < zero || charCode > nine) break;
              }
            }

            c = code[extent];
            if (c === "N" || c === "M") extent++;

            tokens[tn++] = new Token(code.slice(i, extent), i, "value", extent - i);
            break;
          }

        case "_":
          if (dispatch && c === "_") {
            tokens[tn++] = new Token(code.slice(i, ++extent), i, "skip", extent - i);
            break;
          } // if not a skip, fall through to symbols

        // Allow just about any other symbol as a symbol. This is far more permissive than
        // clojure actually allows, but should catch any weirdo crap that accidentally gets
        // into the code.
        default:
          for (extent++; extent <= j; extent++) {
            switch (code[extent]) {
              case " ":
              case "\t":
              case "\n":
              case "\r":
              case "\\":
              case ",":
              case "{":
              case "}":
              case "(":
              case ")":
              case "[":
              case "]":
              case "^":
              case "`":
              case "@":
                break;
              case ";":
                // theres a weird bug via syntax highligher that gives us escaped entities.
                // need to watch out for these
                if (code.slice(extent-3, extent+1) === "&lt;"
                    ||code.slice(extent-3, extent+1) === "&gt;"
                    ||code.slice(extent-4, extent+1) === "&amp;") {
                  continue;
                }
                break;
              default:
                continue;
            }
            break;
          }

          var value = code.slice(i, extent);
          var tag = "symbol";
          if (value[0] == ":") {
            tag = "keyword";
          }
          else if (value === "true" || value === "false" || value === "nil") {
            tag = "value";
          }
          tokens[tn++] = new Token(value, i, tag, extent - i);
      }

      dispatch = false;
      i = extent;
    }

    return tokens;
  }


  function build_tree(tokens) {
    var toplevel = {
      list: [],
      tag: "toplevel",
      parent: null,
      opening: null,
      closing: null,
      depth: -1
    };

    // loop variables hoisted out as semi globals to track position in token stream
    var i = -1;
    var j = tokens.length;

    var short_fn = false; // are we already inside a #( … ) function form?

    function parse_one(t) {
      // ignore special tokens and forms that dont belong in the tree
      for (; t && (t.tag === "comments" || t.tag === "invalid" || t.tag == "skip") && i < j; ) {
        if (t.tag === "skip") {
          t.tag = "preprocessor";
          annotate_comment(parse_one(tokens[++i]));
        }
        t = tokens[++i];
      }

      if (!t) return {}; // hackity hack

      switch (t.tag) {
        case "{":
          return build_aggregate(new LispNode("map", [], t), "}");
        case "(":
          return build_aggregate(new LispNode("list", [], t), ")");
        case "#{":
          return build_aggregate(new LispNode("set", [], t), "}");
        case "[":
          return build_aggregate(new LispNode("vector", [], t), "]");
        case "#(": // this is a bit hairy, but it annotates nested #( … ) forms as invalid
          var prev_short_fn = short_fn;
          try {
            short_fn = true;
            var aggregate = build_aggregate(new LispNode("list", [], t), ")");
            if (prev_short_fn) {
              aggregate.opening.tag = "invalid";
              aggregate.closing.tag = "invalid";
            }
            return aggregate;
          }
          finally {
            short_fn = prev_short_fn;
          }
        case "'":
          return new PrefixNode("quote", t, parse_one(tokens[++i]));
        case "#'":
          return new PrefixNode("varquote", t, parse_one(tokens[++i]));
        case "@":
          return new PrefixNode("deref", t, parse_one(tokens[++i]));
        case "`":
          return new PrefixNode("syntaxquote", t, parse_one(tokens[++i]));
        case "unquote":
          return new PrefixNode("unquote", t, parse_one(tokens[++i]));
        case "splice":
          return new PrefixNode("splice", t, parse_one(tokens[++i]));
        case "^":
          t.tag = "meta";
          var meta = parse_one(tokens[++i]);
          var next = parse_one(tokens[++i]);
          next.meta = meta;
          return next;
      }

      return t;
    }

    // build_aggregate collects to ether sub forms for one aggregate for.
    function build_aggregate(current, expected_closing) {
      for (i++; i < j; i++) {
        var t = tokens[i];

        if (t.tag === "}" || t.tag === ")" || t.tag === "]") {
          if (t.tag !== expected_closing) t.tag = "invalid";
          current.closing = t;
          if (expected_closing) return current;
        }
        var node = parse_one(t);

        node.parent = current;
        current.list[current.list.length] = node;
      }

      return current;
    }

    build_aggregate(toplevel, null);

    return toplevel;
  }

  // annotation rules to apply to a form based on its head

  var show_locals = true;  // HACK. would rather not use a (semi)-global.

  /* annotate_comment is a special case annotation.
   * in addition to its role in styling specific forms, it is called by parse_one to
   * ignore any forms skipped with #_
   */
  function annotate_comment(exp) {
    exp.tag = "comments";

    if (exp.list) {
      exp.opening.tag = "comments";
      exp.closing.tag = "comments";

      for (var i = 0; i < exp.list.length; i++) {
        var child = exp.list[i];
        if (child.list) {
          annotate_comment(child);
        }
        if (child.attached_node) {
          annotate_comment(child.attached_node);
        }
        else {
          child.tag = "comments";
        }
      }
    }
  }

  /* custom annotation rules are stored here */
  var annotation_rules = {};

  // this function is exposed to allow ad hoc extension of the customisation rules
  function register_annotation_rule(names, rule) {
    for (var i = 0; i < names.length; i++) {
      annotation_rules[names[i]] = rule;
    }
  }


  function annotate_destructuring (exp, scope) {
    if (exp.list) {
      if (exp.tag === "vector") {
        for (var i = 0; i < exp.list.length; i++) {
          annotate_destructuring(exp.list[i], scope);
        }
      }
      else if (exp.tag === "map") {
        for (var i = 0; i < exp.list.length; i += 2) {
          var key = exp.list[i];
          var val = exp.list[i + 1];

          if (key.tag === "keyword" && val.tag === "vector") {
            for (var ii = 0, jj = val.list.length; ii < jj; ii++) {
              if (val.list[ii].tag !== "symbol") continue;
              val.list[ii].tag = "variable";
              scope[val.list[ii].value] = true;
            }
          }
          else {
            annotate_destructuring(key, scope);
            annotate_expressions(val, scope);
          }
        }
      }
    }
    else if (exp.tag === "symbol" && (exp.value !== "&" && exp.value !== "&amp;")){
      exp.tag = "variable";
      scope[exp.value] = true;
    }
  }

  function _annotate_binding_vector (exp, scope) {
    if (exp.tag !== "vector") return;

    var bindings = exp.list;

    if (bindings.length % 2 === 1) return;

    for (var i = 0; i < bindings.length; i += 2) {
      annotate_destructuring(bindings[i], scope);
      annotate_expressions(bindings[i + 1], scope);
    }
  }

  function annotate_binding (exp, scope) {
    var bindings = exp.list[1];
    if (!show_locals) return; // HACK

    if (bindings) {
      scope = Object.create(scope);
      _annotate_binding_vector(bindings, scope);
    }
    for (var i = 2; i < exp.list.length; i++) {
      annotate_expressions(exp.list[i], scope);
    }
  }

  function _annotate_function_body (exp, scope, start_idx) {
    var argvec = exp.list[start_idx];
    if (argvec.tag !== "vector") return;

    scope = Object.create(scope);

    for (var i = 0, j = argvec.list.length; i < j; i++) {
      annotate_destructuring(argvec.list[i], scope);
    }

    for (var i = start_idx, j = exp.list.length; i < j; i++) {
      annotate_expressions(exp.list[i], scope);
    }
  }

  function annotate_function (exp, scope) {
      if (exp.list == undefined) return;
      for (var i = 1, j = exp.list.length; i < j; i++) {
      var child = exp.list[i];

      if (child.tag === "vector") {
        _annotate_function_body (exp, scope, i);
        return;
      }
      else if (child.tag === "list") {
        _annotate_function_body(child, scope, 0)
      }
    }
  }

  function annotate_letfn (exp, scope) {
      try
      {
          scope = Object.create(scope);
          var bindings = exp.list[1];

          var fn;
          for (var i = 0, j = bindings.list.length; i < j; i++)
          {
              fn = bindings.list[i];
              try
              {
                  if (!fn.list[0]) continue;
              }
              catch (e)
              {
                  continue;
              }
              fn.list[0].tag = "variable";
              scope[fn.list[0].value] = true;
          }

          for (i = 0, j = bindings.list.length; i < j; i++)
          {
              var fn = bindings.list[i];
              annotate_function(fn, scope);
          }

          for (i = 2, j = exp.list.length; i < j; i++)
          {
              annotate_expressions(exp.list[i], scope);
          }
      }
      catch (e)
      {
      }
  }

  register_annotation_rule(
    ["comment"],
    annotate_comment
  );

  register_annotation_rule(
    ["let", "when-let", "if-let", "binding", "doseq", "for", "dotimes", "let*"],
    annotate_binding
  );

  register_annotation_rule(
    ["defn", "defn-", "fn", "bound-fn", "defmacro", "fn*", "defmethod"],
    annotate_function
  );

  register_annotation_rule(
    ["letfn"],
    annotate_letfn
  );

  // standard annotations

  function _annotate_metadata_recursive(meta, scope) {
    if (!meta) return;

    if (meta.list !== undefined && meta.list !== null) {
      for (var i = 0, j = meta.list.length; i < j; i++) {
        meta.opening.secondary_tags.meta = true
        meta.closing.secondary_tags.meta = true
        _annotate_metadata_recursive(meta.list[i], scope);
      }
    }
    else if (meta.attached_node) {
      meta.token.secondary_tags.meta = true;
      _annotate_metadata_recursive(meta.attached_node, scope);
    }
    else {
      meta.secondary_tags.meta = true;
    }
  }

  function annotate_metadata(exp) {
    if (!(exp && exp.meta)) return;
    var meta = exp.meta;

     annotate_expressions(meta, {});
    _annotate_metadata_recursive(meta, {});
  }


  function annotate_quoted(exp, scope) {
    if (!exp) return;

    if (exp.list !== undefined && exp.list !== null) {
      for (var i = 0, j = exp.list.length; i < j; i++) {
        exp.opening.secondary_tags.quoted = true
        exp.closing.secondary_tags.quoted = true
        annotate_quoted(exp.list[i], scope);
      }
    }
    else if (exp.attached_node) {
      if (exp.tag === "unquote" || exp.tag === "splice") return;
      exp.token.secondary_tags.quoted = true;
      annotate_quoted(exp.attached_node, scope);
    }
    else {
      exp.secondary_tags.quoted = true;
    }
  }


  function annotate_expressions(exp, scope) {
    annotate_metadata(exp);

    switch (exp.tag) {
      case "toplevel":
        for (var i = 0; i < exp.list.length; i++) {
          annotate_expressions(exp.list[i], scope);
        }
        break;

      case "list": // functions, macros, special forms, comments
        var head = exp.list[0];

        if (head) {
          if (head.tag === "list" || head.tag === "vector"
           || head.tag === "map" || head.tag === "set") {
            annotate_expressions(head, scope);
          }
          else if (head.attached_node) {
            annotate_expressions(head.attached_node, scope);
          }
          else {
            head.tag = (head.value.match(/(^\.)|(\.$)|[A-Z].*\//)
                        ? "method"
                        : "function");
          }

          // apply specific rules
          if (annotation_rules.hasOwnProperty(head.value)) {
            annotation_rules[head.value](exp, scope);
          }
          else {
            for (var i = 1; i < exp.list.length; i++) {
              annotate_expressions(exp.list[i], scope);
            }
          }
        }
        else { // empty list
          exp.opening.tag = "value";
          exp.closing.tag = "value";
        }

        break;

      case "vector": // data
      case "map":
      case "set":
        for (var i = 0; i < exp.list.length; i++) {
          annotate_expressions(exp.list[i], scope);
        }
        break;

      case "symbol":
        if (exp.value.match(/[A-Z].*\/[A-Z_]+/)) {
          exp.tag = "constant";
        }
        else if (show_locals && scope[exp.value]) {
          exp.tag = "variable";
        }
        else if (exp.tag === "symbol" && exp.value.match(/([A-Z].*\/)?[A-Z_]+/)) {
          exp.tag = "type";
        }
        break;

      case "quote":
      case "syntaxquote":
        annotate_quoted(exp.attached_node, scope);

      default:
        if (exp.attached_node) annotate_expressions(exp.attached_node, scope);
    }
  }

  // translation of tag to css:
  var css_translation = {
    "constant":     "constants",
    "keyword":      "constants",
    "method":       "color1",
    "type":         "color3",
    "function":     "functions",
    "string":       "string",
    "regexp":       "string",
    "value":        "value",
    "comments":     "comments",
    "symbol":       "symbol",
    "variable":     "variable",
    "splice":       "preprocessor",
    "unquote":      "preprocessor",
    "preprocessor": "preprocessor",
    "meta":         "preprocessor",
    "'":            "preprocessor",
    "#'":           "preprocessor",
    "(":            "plain",
    ")":            "plain",
    "{":            "keyword",
    "}":            "keyword",
    "#{":           "keyword",
    "[":            "keyword",
    "]":            "keyword",
    "invalid":      "invalid",
    "@":            "plain"
  };

  function translate_tags_to_css(tokens) {
    for (var i = 0, j = tokens.length; i < j; i++) {
      var token = tokens[i];
      token.css = css_translation[token.tag];
      for (var k in token.secondary_tags) if (token.secondary_tags.hasOwnProperty(k))
        token.css += " " + k ;
    };
  }


  // create the new brush

  SH.brushes.Clojure = function () {};
  SH.brushes.Clojure.prototype = new SyntaxHighlighter.Highlighter();

  SH.brushes.Clojure.prototype.findMatches = function find_matches (regexpList, code) {
    // this is a nasty global hack. need to resolve this
    if (this.params && this.params.locals) {
      show_locals = this.params.locals === true || this.params.locals === "true";
    }
    else {
      show_locals = true;
    }

    var tokens = tokenize(code);
    annotate_expressions(build_tree(tokens), {});
    translate_tags_to_css(tokens);

    return tokens;
  };

  SH.brushes.Clojure.aliases = ['clojure', 'Clojure', 'clj'];
  SH.brushes.Clojure.register_annotation_rule = register_annotation_rule;

  return {
    tokenize: tokenize,
    build_tree: build_tree
  };
})(SyntaxHighlighter);
