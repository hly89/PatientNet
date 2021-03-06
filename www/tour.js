/* ========================================================================
 * bootstrap-tour - v0.10.3
 * http://bootstraptour.com
 * ========================================================================
 * Copyright 2012-2015 Ulrich Sossou
 *
 * ========================================================================
 * Licensed under the MIT License (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/MIT
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ========================================================================
 */
 
!function(a,b){return"function"==typeof define&&define.amd?define(["jquery"],function(c){return a.Tour=b(c)}):"object"==typeof exports?module.exports=b(require("jQuery")):a.Tour=b(a.jQuery)}(window,function(a){var b,c;return c=window.document,b=function(){function b(b){var c;try{c=window.localStorage}catch(a){c=!1}this._options=a.extend({name:"tour",steps:[],container:"body",autoscroll:!0,keyboard:!0,storage:c,debug:!1,backdrop:!1,backdropContainer:"body",backdropPadding:0,redirect:!0,orphan:!1,duration:!1,delay:!1,basePath:"",template:'<div class="popover" role="tooltip"> <div class="arrow"></div> <h3 class="popover-title"></h3> <div class="popover-content"></div> <div class="popover-navigation"> <div class="btn-group"> <button class="btn btn-sm btn-default" data-role="prev">&laquo; Prev</button> <button class="btn btn-sm btn-default" data-role="next">Next &raquo;</button> <button class="btn btn-sm btn-default" data-role="pause-resume" data-pause-text="Pause" data-resume-text="Resume">Pause</button> </div> <button class="btn btn-sm btn-default" data-role="end">End tour</button> </div> </div>',afterSetState:function(a,b){},afterGetState:function(a,b){},afterRemoveState:function(a){},onStart:function(a){},onEnd:function(a){},onShow:function(a){},onShown:function(a){},onHide:function(a){},onHidden:function(a){},onNext:function(a){},onPrev:function(a){},onPause:function(a,b){},onResume:function(a,b){},onRedirectError:function(a){}},b),this._force=!1,this._inited=!1,this._current=null,this.backdrop={overlay:null,$element:null,$background:null,backgroundShown:!1,overlayElementShown:!1}}return b.prototype.addSteps=function(a){var b,c,d;for(c=0,d=a.length;c<d;c++)b=a[c],this.addStep(b);return this},b.prototype.addStep=function(a){return this._options.steps.push(a),this},b.prototype.getStep=function(b){if(null!=this._options.steps[b])return a.extend({id:"step-"+b,path:"",host:"",placement:"right",title:"",content:"<p></p>",next:b===this._options.steps.length-1?-1:b+1,prev:b-1,animation:!0,container:this._options.container,autoscroll:this._options.autoscroll,backdrop:this._options.backdrop,backdropContainer:this._options.backdropContainer,backdropPadding:this._options.backdropPadding,redirect:this._options.redirect,reflexElement:this._options.steps[b].element,backdropElement:this._options.steps[b].element,orphan:this._options.orphan,duration:this._options.duration,delay:this._options.delay,template:this._options.template,onShow:this._options.onShow,onShown:this._options.onShown,onHide:this._options.onHide,onHidden:this._options.onHidden,onNext:this._options.onNext,onPrev:this._options.onPrev,onPause:this._options.onPause,onResume:this._options.onResume,onRedirectError:this._options.onRedirectError},this._options.steps[b])},b.prototype.init=function(a){return this._force=a,this.ended()?(this._debug("Tour ended, init prevented."),this):(this.setCurrentStep(),this._initMouseNavigation(),this._initKeyboardNavigation(),this._onResize(function(a){return function(){return a.showStep(a._current)}}(this)),null!==this._current&&this.showStep(this._current),this._inited=!0,this)},b.prototype.start=function(a){var b;return null==a&&(a=!1),this._inited||this.init(a),null===this._current&&(b=this._makePromise(null!=this._options.onStart?this._options.onStart(this):void 0),this._callOnPromiseDone(b,this.showStep,0)),this},b.prototype.next=function(){var a;return a=this.hideStep(this._current,this._current+1),this._callOnPromiseDone(a,this._showNextStep)},b.prototype.prev=function(){var a;return a=this.hideStep(this._current,this._current-1),this._callOnPromiseDone(a,this._showPrevStep)},b.prototype.goTo=function(a){var b;return b=this.hideStep(this._current,a),this._callOnPromiseDone(b,this.showStep,a)},b.prototype.end=function(){var b,d;return b=function(b){return function(d){if(a(c).off("click.tour-"+b._options.name),a(c).off("keyup.tour-"+b._options.name),a(window).off("resize.tour-"+b._options.name),b._setState("end","yes"),b._inited=!1,b._force=!1,b._clearTimer(),null!=b._options.onEnd)return b._options.onEnd(b)}}(this),d=this.hideStep(this._current),this._callOnPromiseDone(d,b)},b.prototype.ended=function(){return!this._force&&!!this._getState("end")},b.prototype.restart=function(){return this._removeState("current_step"),this._removeState("end"),this._removeState("redirect_to"),this.start()},b.prototype.pause=function(){var a;return a=this.getStep(this._current),a&&a.duration?(this._paused=!0,this._duration-=(new Date).getTime()-this._start,window.clearTimeout(this._timer),this._debug("Paused/Stopped step "+(this._current+1)+" timer ("+this._duration+" remaining)."),null!=a.onPause?a.onPause(this,this._duration):void 0):this},b.prototype.resume=function(){var a;return a=this.getStep(this._current),a&&a.duration?(this._paused=!1,this._start=(new Date).getTime(),this._duration=this._duration||a.duration,this._timer=window.setTimeout(function(a){return function(){return a._isLast()?a.next():a.end()}}(this),this._duration),this._debug("Started step "+(this._current+1)+" timer with duration "+this._duration),null!=a.onResume&&this._duration!==a.duration?a.onResume(this,this._duration):void 0):this},b.prototype.hideStep=function(b,c){var d,e,f,g;if(g=this.getStep(b))return this._clearTimer(),f=this._makePromise(null!=g.onHide?g.onHide(this,b):void 0),e=function(d){return function(e){var f,h;if(f=a(g.element),f.data("bs.popover")||f.data("popover")||(f=a("body")),f.popover("destroy").removeClass("tour-"+d._options.name+"-element tour-"+d._options.name+"-"+b+"-element").removeData("bs.popover").focus(),g.reflex&&a(g.reflexElement).removeClass("tour-step-element-reflex").off(""+d._reflexEvent(g.reflex)+".tour-"+d._options.name),g.backdrop&&(h=null!=c&&d.getStep(c),h&&h.backdrop&&h.backdropElement===g.backdropElement||d._hideBackdrop()),null!=g.onHidden)return g.onHidden(d)}}(this),d=g.delay.hide||g.delay,"[object Number]"==={}.toString.call(d)&&d>0?(this._debug("Wait "+d+" milliseconds to hide the step "+(this._current+1)),window.setTimeout(function(a){return function(){return a._callOnPromiseDone(f,e)}}(this),d)):this._callOnPromiseDone(f,e),f},b.prototype.showStep=function(a){var b,d,e,f,g,h;return this.ended()?(this._debug("Tour ended, showStep prevented."),this):(h=this.getStep(a),h&&(g=a<this._current,d=this._makePromise(null!=h.onShow?h.onShow(this,a):void 0),this.setCurrentStep(a),b=function(){switch({}.toString.call(h.path)){case"[object Function]":return h.path();case"[object String]":return this._options.basePath+h.path;default:return h.path}}.call(this),!h.redirect||!this._isRedirect(h.host,b,c.location)||(this._redirect(h,a,b),this._isJustPathHashDifferent(h.host,b,c.location)))?(f=function(b){return function(c){var d;if(b._isOrphan(h)){if(h.orphan===!1)return b._debug("Skip the orphan step "+(b._current+1)+".\nOrphan option is false and the element does not exist or is hidden."),void(g?b._showPrevStep():b._showNextStep());b._debug("Show the orphan step "+(b._current+1)+". Orphans option is true.")}if(h.backdrop&&b._showBackdrop(h),d=function(){if(b.getCurrentStep()===a&&!b.ended())return null!=h.element&&h.backdrop&&b._showOverlayElement(h,!0),b._showPopover(h,a),null!=h.onShown&&h.onShown(b),b._debug("Step "+(b._current+1)+" of "+b._options.steps.length)},h.autoscroll?b._scrollIntoView(h,d):d(),h.duration)return b.resume()}}(this),e=h.delay.show||h.delay,"[object Number]"==={}.toString.call(e)&&e>0?(this._debug("Wait "+e+" milliseconds to show the step "+(this._current+1)),window.setTimeout(function(a){return function(){return a._callOnPromiseDone(d,f)}}(this),e)):this._callOnPromiseDone(d,f),d):void 0)},b.prototype.getCurrentStep=function(){return this._current},b.prototype.setCurrentStep=function(a){return null!=a?(this._current=a,this._setState("current_step",a)):(this._current=this._getState("current_step"),this._current=null===this._current?null:parseInt(this._current,10)),this},b.prototype.redraw=function(){return this._showOverlayElement(this.getStep(this.getCurrentStep()).element,!0)},b.prototype._setState=function(a,b){var c,d;if(this._options.storage){d=""+this._options.name+"_"+a;try{this._options.storage.setItem(d,b)}catch(a){c=a,c.code===DOMException.QUOTA_EXCEEDED_ERR&&this._debug("LocalStorage quota exceeded. State storage failed.")}return this._options.afterSetState(d,b)}return null==this._state&&(this._state={}),this._state[a]=b},b.prototype._removeState=function(a){var b;return this._options.storage?(b=""+this._options.name+"_"+a,this._options.storage.removeItem(b),this._options.afterRemoveState(b)):null!=this._state?delete this._state[a]:void 0},b.prototype._getState=function(a){var b,c;return this._options.storage?(b=""+this._options.name+"_"+a,c=this._options.storage.getItem(b)):null!=this._state&&(c=this._state[a]),void 0!==c&&"null"!==c||(c=null),this._options.afterGetState(a,c),c},b.prototype._showNextStep=function(){var a,b,c;return c=this.getStep(this._current),b=function(a){return function(b){return a.showStep(c.next)}}(this),a=this._makePromise(null!=c.onNext?c.onNext(this):void 0),this._callOnPromiseDone(a,b)},b.prototype._showPrevStep=function(){var a,b,c;return c=this.getStep(this._current),b=function(a){return function(b){return a.showStep(c.prev)}}(this),a=this._makePromise(null!=c.onPrev?c.onPrev(this):void 0),this._callOnPromiseDone(a,b)},b.prototype._debug=function(a){if(this._options.debug)return window.console.log("Bootstrap Tour '"+this._options.name+"' | "+a)},b.prototype._isRedirect=function(a,b,c){var d;return!(null==a||""===a||!("[object RegExp]"==={}.toString.call(a)&&!a.test(c.origin)||"[object String]"==={}.toString.call(a)&&this._isHostDifferent(a,c)))||(d=[c.pathname,c.search,c.hash].join(""),null!=b&&""!==b&&("[object RegExp]"==={}.toString.call(b)&&!b.test(d)||"[object String]"==={}.toString.call(b)&&this._isPathDifferent(b,d)))},b.prototype._isHostDifferent=function(a,b){switch({}.toString.call(a)){case"[object RegExp]":return!a.test(b.origin);case"[object String]":return this._getProtocol(a)!==this._getProtocol(b.href)||this._getHost(a)!==this._getHost(b.href);default:return!0}},b.prototype._isPathDifferent=function(a,b){return this._getPath(a)!==this._getPath(b)||!this._equal(this._getQuery(a),this._getQuery(b))||!this._equal(this._getHash(a),this._getHash(b))},b.prototype._isJustPathHashDifferent=function(a,b,c){var d;return(null==a||""===a||!this._isHostDifferent(a,c))&&(d=[c.pathname,c.search,c.hash].join(""),"[object String]"==={}.toString.call(b)&&(this._getPath(b)===this._getPath(d)&&this._equal(this._getQuery(b),this._getQuery(d))&&!this._equal(this._getHash(b),this._getHash(d))))},b.prototype._redirect=function(b,d,e){var f;return a.isFunction(b.redirect)?b.redirect.call(this,e):(f="[object String]"==={}.toString.call(b.host)?""+b.host+e:e,this._debug("Redirect to "+f),this._getState("redirect_to")!==""+d?(this._setState("redirect_to",""+d),c.location.href=f):(this._debug("Error redirection loop to "+e),this._removeState("redirect_to"),null!=b.onRedirectError?b.onRedirectError(this):void 0))},b.prototype._isOrphan=function(b){return null==b.element||!a(b.element).length||a(b.element).is(":hidden")&&"http://www.w3.org/2000/svg"!==a(b.element)[0].namespaceURI},b.prototype._isLast=function(){return this._current<this._options.steps.length-1},b.prototype._showPopover=function(b,c){var d,e,f,g,h;if(a(".tour-"+this._options.name).remove(),g=a.extend({},this._options),f=this._isOrphan(b),b.template=this._template(b,c),f&&(b.element="body",b.placement="top"),d=a(b.element),d.addClass("tour-"+this._options.name+"-element tour-"+this._options.name+"-"+c+"-element"),b.options&&a.extend(g,b.options),b.reflex&&!f&&a(b.reflexElement).addClass("tour-step-element-reflex").off(""+this._reflexEvent(b.reflex)+".tour-"+this._options.name).on(""+this._reflexEvent(b.reflex)+".tour-"+this._options.name,function(a){return function(){return a._isLast()?a.next():a.end()}}(this)),h=b.smartPlacement===!0&&b.placement.search(/auto/i)===-1,d.popover({placement:h?"auto "+b.placement:b.placement,trigger:"manual",title:b.title,content:b.content,html:!0,animation:b.animation,container:b.container,template:b.template,selector:b.element}).popover("show"),e=d.data("bs.popover")?d.data("bs.popover").tip():d.data("popover").tip(),e.attr("id",b.id),this._focus(e,d,b.next<0),this._reposition(e,b),f)return this._center(e)},b.prototype._template=function(b,c){var d,e,f,g,h,i;return i=b.template,this._isOrphan(b)&&"[object Boolean]"!=={}.toString.call(b.orphan)&&(i=b.orphan),h=a(a.isFunction(i)?i(c,b):i),d=h.find(".popover-navigation"),f=d.find('[data-role="prev"]'),e=d.find('[data-role="next"]'),g=d.find('[data-role="pause-resume"]'),this._isOrphan(b)&&h.addClass("orphan"),h.addClass("tour-"+this._options.name+" tour-"+this._options.name+"-"+c),b.reflex&&h.addClass("tour-"+this._options.name+"-reflex"),b.prev<0&&f.addClass("disabled").prop("disabled",!0).prop("tabindex",-1),b.next<0&&e.addClass("disabled").prop("disabled",!0).prop("tabindex",-1),b.duration||g.remove(),h.clone().wrap("<div>").parent().html()},b.prototype._reflexEvent=function(a){return"[object Boolean]"==={}.toString.call(a)?"click":a},b.prototype._focus=function(a,b,c){var d,e;return e=c?"end":"next",d=a.find("[data-role='"+e+"']"),b.on("shown.bs.popover",function(){return d.focus()})},b.prototype._reposition=function(b,d){var e,f,g,h,i,j,k;if(h=b[0].offsetWidth,f=b[0].offsetHeight,k=b.offset(),i=k.left,j=k.top,e=a(c).outerHeight()-k.top-b.outerHeight(),e<0&&(k.top=k.top+e),g=a("html").outerWidth()-k.left-b.outerWidth(),g<0&&(k.left=k.left+g),k.top<0&&(k.top=0),k.left<0&&(k.left=0),b.offset(k),"bottom"===d.placement||"top"===d.placement){if(i!==k.left)return this._replaceArrow(b,2*(k.left-i),h,"left")}else if(j!==k.top)return this._replaceArrow(b,2*(k.top-j),f,"top")},b.prototype._center=function(b){return b.css("top",a(window).outerHeight()/2-b.outerHeight()/2)},b.prototype._replaceArrow=function(a,b,c,d){return a.find(".arrow").css(d,b?50*(1-b/c)+"%":"")},b.prototype._scrollIntoView=function(b,c){var d,e,f,g,h,i,j;if(d=a(b.element),!d.length)return c();switch(e=a(window),h=d.offset().top,g=d.outerHeight(),j=e.height(),i=0,b.placement){case"top":i=Math.max(0,h-j/2);break;case"left":case"right":i=Math.max(0,h+g/2-j/2);break;case"bottom":i=Math.max(0,h+g-j/2)}return this._debug("Scroll into view. ScrollTop: "+i+". Element offset: "+h+". Window height: "+j+"."),f=0,a("body, html").stop(!0,!0).animate({scrollTop:Math.ceil(i)},function(a){return function(){if(2===++f)return c(),a._debug("Scroll into view.\nAnimation end element offset: "+d.offset().top+".\nWindow height: "+e.height()+".")}}(this))},b.prototype._onResize=function(b,c){return a(window).on("resize.tour-"+this._options.name,function(){return clearTimeout(c),c=setTimeout(b,100)})},b.prototype._initMouseNavigation=function(){var b;return b=this,a(c).off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='prev']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='next']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='end']").off("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='pause-resume']").on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='next']",function(a){return function(b){return b.preventDefault(),a.next()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='prev']",function(a){return function(b){if(b.preventDefault(),a._current>0)return a.prev()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='end']",function(a){return function(b){return b.preventDefault(),a.end()}}(this)).on("click.tour-"+this._options.name,".popover.tour-"+this._options.name+" *[data-role='pause-resume']",function(c){var d;return c.preventDefault(),d=a(this),d.text(b._paused?d.data("pause-text"):d.data("resume-text")),b._paused?b.resume():b.pause()})},b.prototype._initKeyboardNavigation=function(){if(this._options.keyboard)return a(c).on("keyup.tour-"+this._options.name,function(a){return function(b){if(b.which)switch(b.which){case 39:return b.preventDefault(),a._isLast()?a.next():a.end();case 37:if(b.preventDefault(),a._current>0)return a.prev()}}}(this))},b.prototype._makePromise=function(b){return b&&a.isFunction(b.then)?b:null},b.prototype._callOnPromiseDone=function(a,b,c){return a?a.then(function(a){return function(d){return b.call(a,c)}}(this)):b.call(this,c)},b.prototype._showBackdrop=function(b){if(!this.backdrop.backgroundShown)return this.backdrop=a("<div>",{class:"tour-backdrop"}),this.backdrop.backgroundShown=!0,a(b.backdropContainer).append(this.backdrop)},b.prototype._hideBackdrop=function(){return this._hideOverlayElement(),this._hideBackground()},b.prototype._hideBackground=function(){if(this.backdrop&&this.backdrop.remove)return this.backdrop.remove(),this.backdrop.overlay=null,this.backdrop.backgroundShown=!1},b.prototype._showOverlayElement=function(b,c){var d,e,f;if(e=a(b.element),d=a(b.backdropElement),e&&0!==e.length&&(!this.backdrop.overlayElementShown||c))return this.backdrop.overlayElementShown||(this.backdrop.$element=d.addClass("tour-step-backdrop"),this.backdrop.$background=a("<div>",{class:"tour-step-background"}),this.backdrop.$background.appendTo(b.backdropContainer),this.backdrop.overlayElementShown=!0),f={width:d.innerWidth(),height:d.innerHeight(),offset:d.offset()},b.backdropPadding&&(f=this._applyBackdropPadding(b.backdropPadding,f)),this.backdrop.$background.width(f.width).height(f.height).offset(f.offset)},b.prototype._hideOverlayElement=function(){if(this.backdrop.overlayElementShown)return this.backdrop.$element.removeClass("tour-step-backdrop"),this.backdrop.$background.remove(),this.backdrop.$element=null,this.backdrop.$background=null,this.backdrop.overlayElementShown=!1},b.prototype._applyBackdropPadding=function(a,b){return"object"==typeof a?(null==a.top&&(a.top=0),null==a.right&&(a.right=0),null==a.bottom&&(a.bottom=0),null==a.left&&(a.left=0),b.offset.top=b.offset.top-a.top,b.offset.left=b.offset.left-a.left,b.width=b.width+a.left+a.right,b.height=b.height+a.top+a.bottom):(b.offset.top=b.offset.top-a,b.offset.left=b.offset.left-a,b.width=b.width+2*a,b.height=b.height+2*a),b},b.prototype._clearTimer=function(){return window.clearTimeout(this._timer),this._timer=null,this._duration=null},b.prototype._getProtocol=function(a){return a=a.split("://"),a.length>1?a[0]:"http"},b.prototype._getHost=function(a){return a=a.split("//"),a=a.length>1?a[1]:a[0],a.split("/")[0]},b.prototype._getPath=function(a){return a.replace(/\/?$/,"").split("?")[0].split("#")[0]},b.prototype._getQuery=function(a){return this._getParams(a,"?")},b.prototype._getHash=function(a){return this._getParams(a,"#")},b.prototype._getParams=function(a,b){var c,d,e,f,g;if(d=a.split(b),1===d.length)return{};for(d=d[1].split("&"),e={},f=0,g=d.length;f<g;f++)c=d[f],c=c.split("="),e[c[0]]=c[1]||"";return e},b.prototype._equal=function(a,b){var c,d,e,f,g,h;if("[object Object]"==={}.toString.call(a)&&"[object Object]"==={}.toString.call(b)){if(d=Object.keys(a),e=Object.keys(b),d.length!==e.length)return!1;for(c in a)if(f=a[c],!this._equal(b[c],f))return!1;return!0}if("[object Array]"==={}.toString.call(a)&&"[object Array]"==={}.toString.call(b)){if(a.length!==b.length)return!1;for(c=g=0,h=a.length;g<h;c=++g)if(f=a[c],!this._equal(f,b[c]))return!1;return!0}return a===b},b}()});

var tourused = 0;

var tour = new Tour({
	  storage: false,
	  orphan: true,
	  onEnd: function (tour) { var aaa=Math.round((new Date).getTime()/1e3); Shiny.onInputChange("endtour", { aaa }); $("#wraptour").show();}
  })

$( document ).ready(function() {

    tour.addSteps([
      {
      element: "#buttonTour",
      title: "Welcome!", 
      content: "Welcome to PatientNet. <br>Now let's walk through it step by step with the example data.",
	  placement: "bottom"
	  },
  { 
    element: "#filetype",
    title: "Choose input file type!",
    content: "Before uploading your input file or files, please choose the input file type: one single input file or three separate input files. Please read <b> data preparation</b> in <a>https://github.com/hly89/PatientNet</a>."
  },
  
  {
	    element:"#singleinput",
	    title: "Upload input",
	    content: "Please upload a single xlsl file with four sheets: Target, DSS, Mutation, and Gene.exp.",
	    onShown: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("typetour", { aaa });}
	  },
	  

	  {
	    element: "showdata",
	    title:"Check input data",
	    content:"All the input data are shown here.",
	    onShown: function (tour) {
	    var aaa = Math.round(new Date().getTime()/1e3); 
	    $("#wraptour").hide();
	    Shiny.onInputChange("datatab", { aaa });
	    }
	    
	  },
	    {
	    element:"#net",
	    title: "Construct network!",
	    content: "Click the button to construct network!"
	  },
	  
	  {
	    element:"#networks",
	    title:"Network",
	    content: "Patient specific cancer vulnerability network!",
	    onShown: function (tour) { var aaa = Math.round(new Date().getTime()/1e3); 
	    Shiny.onInputChange("loadnet", { aaa }); },
	    placement: "left"
	  },
	  
	  {
	    element:"#save",
	    title:"Save results",
	    content: "Click the save button to save the results!",
	    placement: "right"
	  },
	  

  
  {
    element: "#loadExData_small",
    title: "Example data!",
    content: "You can find example data here.",
	placement: "right"  
  },
  {
    element: "#noelementhaha",
    title: "",
    content: "Thank you!"  
  }
  
  //Once the method is chosen you can can calculate synergy scores and visualize synergy maps"
 ]);

 // Initialize the tour
 tour.init();
 
 document.getElementById("buttonTour").addEventListener("click",function(){0==tourused?tour.start():tour.restart(),tourused++},!1);
 
// Toggle Buttons// table/matrix
$('.t-toggle-button__option').click(function () {
  $(this).addClass('js-checked').siblings().removeClass('js-checked');
  var aaa = $(".t-toggle-button__option.js-checked span").text(); 
  Shiny.onInputChange("inputDatatype", { aaa })
});
 
})

 function openvideo() {
     var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("openmyvideo", { aaa }); 
 }
  function techdoc() {
     var aaa = Math.round(new Date().getTime()/1e3); Shiny.onInputChange("opentechdoc", { aaa }); 
 }


