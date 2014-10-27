module.exports = {
  "mongodb": {
		"connectionstring": process.env.MONGODB || "mongodb://localhost:27017/test"
	},
	"logger": {
		"api": "logs/api.log",
		"exception": "logs/exceptions.log"
	}
};