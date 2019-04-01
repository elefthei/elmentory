const Sequelize = require('sequelize');

// Option 1: Passing parameters separately
const sequelize = new Sequelize('inventory', 'sa', 'myPassw0rd', {
    host: 'localhost',
    dialect: 'mssql'
});

const Model = Sequelize.Model;

class User extends Model {}
User.init({
  username: Sequelize.STRING,
  birthday: Sequelize.DATE
}, { sequelize });

sequelize.sync()
  .then(() => User.create({
    username: 'janedoe',
    birthday: new Date(1980, 6, 20)
  }))
  .then(jane => {
    console.log(jane.toJSON());
  });

