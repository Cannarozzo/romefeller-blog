(function($){
  $(function(){
    
    $('.button-collapse').sideNav();
    $('.scrollspy').scrollSpy();
      
    // MODAL DE LOGIN
    $(document).ready(function(){
      $('.modal-trigger').leanModal();
    });
      
    // Personalizando o Modal
    $('.modal-trigger').leanModal({
      dismissible: true,
      opacity: .5,
      in_duration: 300,
      out_duration: 200,
      ready: function() { console.log('Modal aberto'); }, // Callback Modal aberto
      complete: function() { console.log('Modal fechado'); } // Callback Modal fechado
    });

  }); // end of document ready
})(jQuery); // end of jQuery name space
