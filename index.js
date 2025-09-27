const express = require('express');
const app = express();
app.use(express.static('client/dist'));
app.listen(4173, () => console.log('listening 4173'));
