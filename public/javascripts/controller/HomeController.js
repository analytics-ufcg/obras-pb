angular.module('obrasPb').controller('HomeController', ['ObrasService', function (ObrasService) {
    var self = this;

    this.selectedObras = {};


    this.tableQuery = {
        order : 'dt_Ano',
        limit : 5,
        page : 1
    };

    this.getStartElement = function (page, limit) {
        return page * limit - limit + 1;
    };

    this.getObras = function () {
        console.log('chamous');
        self.obrasPromise = ObrasService.getObras(
            self.tableQuery.limit,
            self.getStartElement(self.tableQuery.page, self.tableQuery.limit)
        ).then(function (data) {
            console.log(data);
            self.obras = data.data;
        }).
          catch(function (err) {
            console.log(err);
        })

    };

    self.getObras();

}]);