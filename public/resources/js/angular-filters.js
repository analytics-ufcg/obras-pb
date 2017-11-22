app.filter('capitalize', function() {
    return function(input) {
        if (input!=null) {
            input = input.toLowerCase().trim();
        return input.substring(0,1).toUpperCase()+input.substring(1);}
        return input
    }
});