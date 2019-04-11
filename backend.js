const Sequelize = require('sequelize');

// Option 1: Passing parameters separately
const sequelize = new Sequelize('inventory', 'sa', '@odyssey1', {
    host: 'ocs-sql-inv.ocs.edu',
    dialect: 'mssql'
});

// Test connection
sequelize
  .authenticate()
  .then(() => {
    console.log('Connection has been established successfully.');
  })
  .catch(err => {
    alert('Unable to connect to the database:', err);
  });

const Model = Sequelize.Model;

class Row extends Model {}
Row.init({
  product: Sequelize.INTEGER,
  distributor: Sequelize.INTEGER,
  date: Sequelize.STRING,
  order: Sequelize.INTEGER,
  description: Sequelize.STRING,
  received: Sequelize.STRING,   // Elm Set encoded as JSON serielized lists
  lower: Sequelize.STRING,      // Here too
  inter: Sequelize.STRING,      // Here too
  middle: Sequelize.STRING,     // Here too
  academia: Sequelize.STRING,   // Here too
  total: Sequelize.INTEGER,
  price: Sequelize.FLOAT
}, { sequelize });

// Commit to SQL db
function commit(entries) {
  sequelize.sync()
    .then(function() {
       let vals = Object.values(entries);
       if (vals === undefined || vals[0] === undefined) {
         throw new Error('Empty commit order given, refusing to do anything');
       } else {
         return Row.findAll({
           where: {
	     order: vals[0].order
	   }
         });
       }
     })
    .then(function(rows) {
       if (rows === undefined || rows.length == 0) {
         Object.keys(entries).forEach(function(key) {
           let row = entries[key];
           Row.create({
	     product: key,
             distributor: row.distributor,
             date: row.date,
             order: row.order,
             description: row.description,
             lower: row.lower,
             inter: row.inter,
             high: row.high,
             academia: row.academia,
             received: row.received,
             total: row.total,
             price: row.price
           });
         });
       } else {
         throw new Error('Order already exists will not be added again');
       }
    })
    .catch((err) =>
      console.log(err)
    );
}

// Load from SQL db
function load(order, callback) {
  sequelize.sync()
    .then(() =>
      Row.findAll({
        where: {
          order: order
        }
    }))
    .then(function(rows){
      callback(rows);
    });
}
