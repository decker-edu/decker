const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = {
  target: "web",
  entry: {
    decker: './src-support/decker.js',
    math: './src-support/math.js',
    plugins: './src-support/rplugins.js',
    classlist: './src-support/classlist.js',
    page: './src-support/page.js',
    handout: './src-support/handout.js',
    three: './src-support/three.js',
    d3: './src-support/d3.js',
    chalkboard: './src-support/chalkboard.js'
  },
  output: {
    path: path.resolve(__dirname, 'resource', 'support'),
    filename: '[name].js'
  },
  devtool: 'cheap-module-eval-source-map',
  module: {
    rules: [{
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      },
      {
        test: /\.s?[ac]ss$/,
        use: [
            {
                loader: MiniCssExtractPlugin.loader,
            },
            {
                loader: 'css-loader',
                options: { sourceMap: true }
            },
            {
                loader: 'sass-loader',
                options: { sourceMap: true }
            }
        ],
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
      },
      {
        test: /\.(png|jp(e*)g|svg)$/,
        use: [{
          loader: 'url-loader',
          options: {
            limit: 8000, // Convert images < 8kb to base64 strings
            name: 'images/[hash]-[name].[ext]'
          }
        }]
      }
    ]
  },
  plugins: [
      new MiniCssExtractPlugin({
          filename: "[name].css"
      })

  ]
};