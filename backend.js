const Sequelize = require('sequelize');

// Option 1: Passing parameters separately
const sequelize = new Sequelize('inventory', 'sa', 'myPassw0rd', {
    host: 'localhost',
    dialect: 'mssql'
});

const Model = Sequelize.Model;

class Row extends Model {}
Row.init({
  product: Sequelize.BIGINT,
  distributor: Sequelize.BIGINT,
  date: Sequelize.DATE,
  order: Sequelize.BIGINT,
  description: Sequelize.STRING,
  received: Sequelize.STRING, // Elm Set encoded as JSON serielized lists
  used: Sequelize.STRING,     // Here too
  total: Sequelize.INT,
  price: Sequelize.FLOAT
}, { sequelize });

function commit(entries) {
  sequelize.sync()
    .then(() =>
      console.log(JSON.stringify(entries)))
    .then(() =>
      Object.keys(entries).forEach(function(key) {
        let row = entries[key]
        Row.create({
	  product: key,
          distributor: row.distributor,
          date: new Date(row.date),
          order: row.order,
          description: row.description,
          received: row.received,
          used: row.used,
          total: row.total,
          price: row.price
        });
      })
    );
}

