const path = require('path');

module.exports = {
  mode: 'development',
  entry: './mbv.js',
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'build')
  },
  devtool: 'source-map',
  performance: {
    hints: false
  }
};
