angular.module('obrasPb').config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {

    $urlRouterProvider.when("", "/home");
    $urlRouterProvider.when("/", "/home");
    $urlRouterProvider.when("/report", "/report");

    $urlRouterProvider.otherwise("/home");

    $stateProvider
        .state('home', {
            url: '/home',
            templateUrl: '/assets/views/home.html',
            controller: 'HomeController as homeCtrl'
        });

    $stateProvider
        .state('report', {
            url: '/report',
            templateUrl: '/assets/views/relatorio-inconsistencias.html',
            controller: 'HomeController as homeCtrl'
        })
}]);
