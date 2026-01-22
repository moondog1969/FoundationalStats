(function($) {
	Drupal.behaviors.equalHeightsModule = {
	  attach: function (context, settings) {
	  var eqClass = Drupal.settings.equalHeightsModule;
    if (eqClass) {
	  //var overflow = Drupal.settings.equalHeightsModule.overflow;
	    $.each(eqClass, function(eqClass, setting) {
		  $('.' + setting.elClass).equalHeights(setting.minheight, setting.maxheight).css('overflow', setting.overflow)});
	  }
    }
	}
})(jQuery);;
/* $Id: nvl_modals.,v 1.0 2012/02/06 23:44:00 nvl.sateesh Exp $ */
(function($) {
	
  Drupal.behaviors.nvlModals = {   
    'attach': function(context, settings) {
      
      $('#modal-content #edit-name').focus();

      /*
       *Removed this code as we are applying a dynamic redirect on login links
      $("a[href*='/user/login'], a[href*='?q=user/login']", context).once('init-modal-forms-login', function () {
        this.href = this.href.replace(/user\/login/,'nvl-modals-login/nojs');
      }).addClass('ctools-use-modal ctools-modal-nvl-modal-style-small');
      */
     
     //lets check if the triggerlogin is true. if yes, then, update the /user/login link
     //to go to the nvl-modals-login/nojs and then add a click event to it so it automatically clicks
     //the click binder is in sites/all/themes/maa/js/custom.js as it works only inside the document.ready
     
      if (getParameter('triggerlogin') == 'true') {
        //let us change the href of Login link to nvl modal js
        $("a[href*='/user/login'], a[href*='?q=user/login']", context).once('init-modal-forms-login', function () {
          this.href = this.href.replace(/user\/login/,'nvl-modals-login/nojs');
        }).addClass('ctools-use-modal ctools-modal-nvl-modal-style-small');

      }else{
        $("a[href*='/user/login'], a[href*='?q=user/login']", context).once('init-modal-forms-login', function () {
          this.href = '/login-secure-redirect';
        });
      }

    }
  }
  
  function getParameter(paramName) {
    var searchString = window.location.search.substring(1),
        i, val, params = searchString.split("&");

    for (i=0;i<params.length;i++) {
      val = params[i].split("=");
      if (val[0] == paramName) {
        return unescape(val[1]);
      }
    }
    return null;
  }  
  
}(jQuery));
;
