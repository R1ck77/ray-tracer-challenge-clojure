# Clojure implementation of "The Ray Tracing Challenge" by Jamis Buck

Made for educational and entertainment purposes. Still work in progress.

The first part of the project was put together deliberately avoiding Object Oriented design and disregarding otimizations in favor of a clean FP approach, as a way to better appreciate both OO and FP (which I did, and I want OO back, just as I expected).

Step two includes OO experimentation with Clojure, and focus of Clojure on records performances is showing. 

Being an educational project and a way to enjoy programming for its own sake, experimentation and fun design are favored above everything else.

## Installation and Usage

This is a leiningen project (If you don't know what this is, check https://leiningen.org/): after you have cloned the project you can get a very quick rendering using the 

    lein with-profile timed-run run

command. 

A bunch of other demos (loosely matching the end of chapter exercises) can be found in the ``demo`` namespace (in the ``tests`` package).

### Bugs

Probably many: use at your own risk.

## License

Copyright © 2019 Riccardo Di Meo

THIS WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Distribution or inclusion of this work as a compiled jar not including the source
code is provided under the Creative Commons CC0 1.0 license, which is available
here: https://creativecommons.org/publicdomain/zero/1.0/legalcode

Other forms of distributions are covered under the terms of the Creative
Commons "Attribution 4.0 International" license, which is available at 
https://creativecommons.org/licenses/by/4.0/legalcode 
