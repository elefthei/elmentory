const Sequelize = require('sequelize');

// Option 1: Passing parameters separately
const sequelize = new Sequelize('inventory', 'sa', 'myPassw0rd', {
    host: 'localhost',
    dialect: 'mssql'
});

const Model = Sequelize.Model;

class Row extends Model {}
Row.init({
  distributor: Sequelize.BIGINT.UNSIGNED,
  date: Sequelize.DATE,
  order: Sequelize.BIGINT.UNSIGNED,
  description: Sequelize.STRING,
  received: Sequelize.STRING, // Elm Set encoded as JSON serielized lists
  used: Sequelize.STRING,     // Here too
  total: Sequelize.INT.UNSIGNED,
  price: Sequelize.FLOAT
}, { sequelize });

app.ports.db.subscribe(function(row) {
  sequelize.sync()
    .then(() => Row.create({
      distributor: row.distributor,
      date: new Date(row.date),
      order: row.order,
      description: row.description,
      received: row.received,
      used: row.used,
      total: row.total,
      price: row.price
    })).then(row => {
      console.log(row.toJSON());
    });
});

