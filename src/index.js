import './main.css';
import { Main } from './Main.elm';

Main.embed(document.getElementById('root'), {
  seed: Date.now() % 20000,
});
