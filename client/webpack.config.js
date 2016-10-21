module.exports = {
    devtool: "source-map",
    entry: "./out/client",
    output: {
        path: __dirname+"/out",
        filename: "bundle.js"
    },    
    module: {
        preLoaders: [{
            test: /\.js$/,
            exclude: /node_modules/,
            loader: "source-map-loader"
        }],
        loaders: [{
            loader: "style-loader!css-loader",
            test: /\.css$/
        }]
    }
};
