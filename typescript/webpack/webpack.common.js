const webpack = require('webpack');
const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

//
// Save output JS in public/generated-js/index.js
//
module.exports = {
  target: 'es5',
  entry: {
    index: path.join(__dirname, '../src/index.ts')
  },
  output: {
    path: path.join(__dirname, '../../public/generated-js'),
    filename: '[name].js'
  },
  module: {
    rules: [
      {
        test: /\.(ts|js)x?$/,
        exclude: /node_modules/,
        loader: 'ts-loader',
      },
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
        ],
      }
    ]
  },
  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.css']
  },
  plugins: [
    new MiniCssExtractPlugin(),
  ]
};
