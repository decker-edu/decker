const path = require('path');

module.exports = {
  target: "web",
  entry: {
    decker: './src-support/decker.js',
    math: './src-support/math.js',
    plugins: './src-support/rplugins.js',
    classlist: './src-support/classlist.js',
    notes: './src-support/notes.js',
    page: './src-support/page.js',
    three: './src-support/three.js',
    d3: './src-support/d3.js'
  },
  output: {
    path: path.resolve(__dirname, 'resource', 'support'),
    filename: '[name].js'
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\.scss$/,
        use: [
          "style-loader",
          "css-loader",
          "sass-loader"
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [{
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            outputPath: 'fonts/'
          }
        }]
      }
    ]
  }
};