$('.box').on('click', '.box-header h3', function() {
    $(this).closest('.box')
           .find('[data-widget=collapse]')
           .click();
});