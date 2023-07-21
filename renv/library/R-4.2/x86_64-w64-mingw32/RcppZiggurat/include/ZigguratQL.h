// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// ZigguratQL.h:  QuantLib version of Ziggurat by Kakhkhor Abdijalilov
//
// Copyright (C) 2013  Dirk Eddelbuettel
//
// This file is part of RcppZiggurat.
//
// RcppZiggurat is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppZiggurat is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppZiggurat.  If not, see <http://www.gnu.org/licenses/>.

// Ziggurat using 2010 Kakhkhor Abdijalilov
// Files: ql/experimental/math/zigguratrng.hpp
//        ql/experimental/math/zigguratrng.cpp
// License: "QuantLib License", 
//          see http://quantlib.org/license.shtml
//          tl;dr -- modified BSD, OSI approved, GPL compatible

/*
 Copyright (C) 2010 Kakhkhor Abdijalilov

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
*/

// this file has GSL depedencies for the Mersenne Twister and the inverse cdf
// we could overcome this by depending on Boost or C++11 instead

#ifndef RcppZiggurat__ZigguratQL_h
#define RcppZiggurat__ZigguratQL_h

#include <mt32wrapper.h>
#include <gsl/gsl_cdf.h>

namespace Ziggurat {
namespace QL {

    typedef double Real;        // QL uses Real as a convention
    typedef std::size_t Size;

    // In old-school C++ these cannot be initialised in the class. In C++11 we could.

    /* The tabulated values were calculated following Marsaglia and Tsang (2000). */

    // values of exp(-0.5*x*x)
    const Real f_ [128] = {
        1.000000000000000E+000, 9.635996931557717E-001,
        9.362826817083744E-001, 9.130436479920410E-001,
        8.922816508023054E-001, 8.732430489268560E-001,
        8.555006078850665E-001, 8.387836053106493E-001,
        8.229072113952640E-001, 8.077382946961230E-001,
        7.931770117838610E-001, 7.791460859417049E-001,
        7.655841739092376E-001, 7.524415591857053E-001,
        7.396772436833397E-001, 7.272569183545073E-001,
        7.151515074204785E-001, 7.033360990258188E-001,
        6.917891434460373E-001, 6.804918410064157E-001,
        6.694276673577075E-001, 6.585820000586550E-001,
        6.479418211185520E-001, 6.374954773431460E-001,
        6.272324852578157E-001, 6.171433708265636E-001,
        6.072195366326060E-001, 5.974531509518134E-001,
        5.878370544418217E-001, 5.783646811267034E-001,
        5.690299910747226E-001, 5.598274127106959E-001,
        5.507517931210564E-001, 5.417983550317252E-001,
        5.329626593899887E-001, 5.242405726789938E-001,
        5.156282382498731E-001, 5.071220510813057E-001,
        4.987186354765854E-001, 4.904148252893227E-001,
        4.822076463348397E-001, 4.740943006982505E-001,
        4.660721526945719E-001, 4.581387162728729E-001,
        4.502916436869279E-001, 4.425287152802475E-001,
        4.348478302546628E-001, 4.272469983095633E-001,
        4.197243320540391E-001, 4.122780401070255E-001,
        4.049064208114891E-001, 3.976078564980433E-001,
        3.903808082413902E-001, 3.832238110598844E-001,
        3.761354695144552E-001, 3.691144536682758E-001,
        3.621594953730338E-001, 3.552693848515477E-001,
        3.484429675498729E-001, 3.416791412350141E-001,
        3.349768533169716E-001, 3.283350983761528E-001,
        3.217529158792090E-001, 3.152293880681579E-001,
        3.087636380092523E-001, 3.023548277894802E-001,
        2.960021568498564E-001, 2.897048604458110E-001,
        2.834622082260129E-001, 2.772735029218981E-001,
        2.711380791410257E-001, 2.650553022581624E-001,
        2.590245673987112E-001, 2.530452985097663E-001,
        2.471169475146971E-001, 2.412389935477517E-001,
        2.354109422657280E-001, 2.296323252343031E-001,
        2.239026993871343E-001, 2.182216465563709E-001,
        2.125887730737364E-001, 2.070037094418741E-001,
        2.014661100762035E-001, 1.959756531181106E-001,
        1.905320403209139E-001, 1.851349970107136E-001,
        1.797842721249623E-001, 1.744796383324025E-001,
        1.692208922389250E-001, 1.640078546849280E-001,
        1.588403711409353E-001, 1.537183122095867E-001,
        1.486415742436971E-001, 1.436100800919331E-001,
        1.386237799858510E-001, 1.336826525846477E-001,
        1.287867061971040E-001, 1.239359802039816E-001,
        1.191305467087186E-001, 1.143705124498883E-001,
        1.096560210158178E-001, 1.049872554103546E-001,
        1.003644410295456E-001, 9.578784912257826E-002,
        9.125780082763474E-002, 8.677467189554304E-002,
        8.233889824295743E-002, 7.795098251465470E-002,
        7.361150188475492E-002, 6.932111739418027E-002,
        6.508058521363191E-002, 6.089077034856640E-002,
        5.675266348153862E-002, 5.266740190350321E-002,
        4.863629586028410E-002, 4.466086220087247E-002,
        4.074286807479065E-002, 3.688438878696881E-002,
        3.308788614650520E-002, 2.935631744025387E-002,
        2.569329193614964E-002, 2.210330461611161E-002,
        1.859210273716583E-002, 1.516729801067205E-002,
        1.183947865798232E-002, 8.624484412930473E-003,
        5.548995220816476E-003, 2.669629083902507E-003
    };

    // acceptance thresholds 2^24*x[i]/x[i+1]. k_[0] is special
    const Size k_[128] = {
        15555141,        0, 12590647, 14272656,
        14988942, 15384587, 15635012, 15807564,
        15933580, 16029597, 16105158, 16166150,
        16216402, 16258511, 16294298, 16325081,
        16351834, 16375294, 16396029, 16414482,
        16431005, 16445883, 16459346, 16471581,
        16482747, 16492974, 16502372, 16511034,
        16519042, 16526462, 16533356, 16539772,
        16545758, 16551351, 16556587, 16561496,
        16566104, 16570437, 16574515, 16578357,
        16581980, 16585401, 16588633, 16591688,
        16594579, 16597314, 16599905, 16602358,
        16604682, 16606885, 16608972, 16610949,
        16612822, 16614597, 16616276, 16617865,
        16619367, 16620786, 16622125, 16623387,
        16624575, 16625690, 16626735, 16627713,
        16628624, 16629470, 16630253, 16630974,
        16631634, 16632233, 16632773, 16633254,
        16633677, 16634041, 16634346, 16634593,
        16634781, 16634910, 16634979, 16634987,
        16634934, 16634817, 16634637, 16634390,
        16634075, 16633689, 16633231, 16632698,
        16632085, 16631390, 16630609, 16629737,
        16628768, 16627698, 16626520, 16625226,
        16623808, 16622257, 16620563, 16618714,
        16616696, 16614494, 16612091, 16609465,
        16606593, 16603449, 16599999, 16596206,
        16592025, 16587402, 16582273, 16576559,
        16570163, 16562965, 16554812, 16545511,
        16534809, 16522368, 16507733, 16490265,
        16469045, 16442690, 16409026, 16364394,
        16302111, 16208408, 16049219, 15707338
    };

    // values of 2^{-24}*x[i]. w_[0] is special.
    static const Real w_[128] = {
        2.213171867573477E-007, 1.623158840564778E-008,
        2.162882274558596E-008, 2.542424120326624E-008,
        2.845751269184242E-008, 3.103351823837397E-008,
        3.330064883086164E-008, 3.534334554922425E-008,
        3.721467240506913E-008, 3.895036212891571E-008,
        4.057573787247544E-008, 4.210946627340346E-008,
        4.356574479471913E-008, 4.495565083232566E-008,
        4.628801273561392E-008, 4.756999377168848E-008,
        4.880749623079987E-008, 5.000544871575862E-008,
        5.116801519263080E-008, 5.229875022755345E-008,
        5.340071633852936E-008, 5.447657412343023E-008,
        5.552865246542405E-008, 5.655900391923845E-008,
        5.756944891143612E-008, 5.856161138431779E-008,
        5.953694781545649E-008, 6.049677105184184E-008,
        6.144227004387700E-008, 6.237452630714050E-008,
        6.329452775023089E-008, 6.420318036567782E-008,
        6.510131817439508E-008, 6.598971173307500E-008,
        6.686907545162751E-008, 6.774007391947947E-008,
        6.860332740181531E-008, 6.945941663712532E-008,
        7.030888704386109E-008, 7.115225242518010E-008,
        7.198999824564194E-008, 7.282258454149729E-008,
        7.365044851627824E-008, 7.447400686528278E-008,
        7.529365786588351E-008, 7.610978326509584E-008,
        7.692274999129007E-008, 7.773291171314836E-008,
        7.854061026581177E-008, 7.934617696152180E-008,
        8.014993379984568E-008, 8.095219459071287E-008,
        8.175326600192373E-008, 8.255344854147119E-008,
        8.335303748390705E-008, 8.415232374905104E-008,
        8.495159474056128E-008, 8.575113515123489E-008,
        8.655122774137352E-008, 8.735215409611426E-008,
        8.815419536728245E-008, 8.895763300505963E-008,
        8.976274948457178E-008, 9.056982903238356E-008,
        9.137915835783214E-008, 9.219102739414587E-008,
        9.300573005436895E-008, 9.382356500725440E-008,
        9.464483647849558E-008, 9.546985508294559E-008,
        9.629893869382930E-008, 9.713241335539087E-008,
        9.797061424595009E-008, 9.881388669897357E-008,
        9.966258729051657E-008, 1.005170850022725E-007,
        1.013777624705017E-007, 1.022450173323223E-007,
        1.031192636822607E-007, 1.040009336536155E-007,
        1.048904791411299E-007, 1.057883736837368E-007,
        1.066951145288121E-007, 1.076112249025135E-007,
        1.085372565144899E-007, 1.094737923296323E-007,
        1.104214496447496E-007, 1.113808835142578E-007,
        1.123527905763905E-007, 1.133379133403490E-007,
        1.143370450055439E-007, 1.153510348970830E-007,
        1.163807946174674E-007, 1.174273050337859E-007,
        1.184916242434419E-007, 1.195748966907839E-007,
        1.206783636434635E-007, 1.218033752829236E-007,
        1.229514047207811E-007, 1.241240643255547E-007,
        1.253231248369812E-007, 1.265505378645533E-007,
        1.278084625218070E-007, 1.290992971506620E-007,
        1.304257173581136E-007, 1.317907219454484E-007,
        1.331976887933646E-007, 1.346504434266883E-007,
        1.361533438964878E-007, 1.377113869008423E-007,
        1.393303418955523E-007, 1.410169225999109E-007,
        1.427790092234294E-007, 1.446259406525023E-007,
        1.465689049606532E-007, 1.486214710528821E-007,
        1.508003278008381E-007, 1.531263366890930E-007,
        1.556260733859904E-007, 1.583341605221148E-007,
        1.612969382476045E-007, 1.645785196056458E-007,
        1.682713836756925E-007, 1.725163463961286E-007,
        1.775441320326934E-007, 1.837747608550914E-007,
        1.921108355867039E-007, 2.051961336074264E-007
    };

    class ZigguratQL : public Zigg {
    public: 
        ZigguratQL(uint32_t seed=12345678) {
            // tail probability
            p_ = 2.880541027242713E-004;
            q_ = 1.0 - p_;

            mt32_.setSeed(seed);
        }
        ~ZigguratQL() {}
        double norm() {
            return nextGaussian();
        }
        void setSeed(const uint32_t seed) {
            mt32_.setSeed(seed);
        }

    private:
        mt32 mt32_;
        Real p_, q_;

        Real nextGaussian() const {

            static const int c[2] = {-1, 1};
            unsigned long i, j;
            int f;
            Real x;

            for (;;) {
                j = mt32_.int32(); 		// generate 32 bits of randomness
                f = j & 1; 			// 1 bit to choose a tails
                j >>= 1;
                i = j & 0x7f; 		// 7 bits to choose a strip
                j >>= 7; 			// the last 24 bits for accepttion/rejection
                x = (c[f]*static_cast<long>(j))*w_[i]; // x is uniform
                // within the i-th strip
                if (j < k_[i]) 		// if true, accept x
                    break;

                // handle rejections
                if (i!=0) { 		// upper strips
                    if ((f_[i-1]-f_[i])*mt32_.int01() + f_[i] < std::exp(-0.5*x*x))
                        break;
                } else { // base strip, sample from the tail
                    //x = c[f]*R::qnorm(p_*mt32_.nextReal()+q_, 0.0, 1.0, FALSE, FALSE);
                    x = c[f] * gsl_cdf_ugaussian_Qinv(1.0 - (p_*mt32_.int01()+q_));
                    break;
                }
            }

            return x;
        }

    };

}
}

#endif
