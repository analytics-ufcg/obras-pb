angular.module('obrasPb').controller('HomeController', ['ObrasService', 'numberOfBuildings', function (ObrasService, numberOfBuildings) {
    var self = this;

    this.selectedObras = {};
    this.numberOfBuildings = numberOfBuildings;


    this.tableQuery = {
        order : 'dt_Ano',
        limit : 5,
        page : 1
    };

    this.getStartElement = function (page, limit) {
        return page * limit - limit + 1;
    };

    this.getObras = function () {
        self.obrasPromise = ObrasService.getObras(
            self.tableQuery.limit,
            self.getStartElement(self.tableQuery.page, self.tableQuery.limit)
        ).then(function (data) {
            self.obras = data.data;
        }).
          catch(function (err) {
            console.log(err);
        })
    };


    self.getObras();

}]);