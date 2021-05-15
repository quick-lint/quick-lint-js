package main

import (
	"fmt"
	"os"
	"os/exec"
	"sync"
)

var Urls = []string{
	"https://github.com/juliangarnier/anime.git",
	"https://github.com/rgalus/sticky-js.git",
	"https://github.com/mciastek/sal.git",
	"https://github.com/malchata/yall.js.git",
	"https://github.com/apexcharts/apexcharts.js.git",
	"https://github.com/rqrauhvmra/Tobi.git",
	"https://github.com/jedrzejchalubek/glidejs.git",
	"https://github.com/jshjohnson/Choices.git",
	"https://github.com/bradley/blotter.git",
	"https://github.com/foliotek/croppie.git",
	"https://github.com/Shopify/draggable.git",
	"https://github.com/cferdinandi/x-ray.git",
	"https://github.com/cferdinandi/drop.git",
	"https://github.com/jlmakes/scrollreveal.js.git",
	"https://github.com/inorganik/CountUp.js.git",
	"https://github.com/agence-webup/dropmic.git",
	"https://github.com/qrohlf/trianglify.git",
	"https://github.com/nathanford/type.js.git",
	"https://github.com/jashkenas/underscore.git",
	"https://github.com/JenkinsDev/Validatinator.git",
	"https://github.com/thephuse/vanilla-modal.git",
	"https://github.com/julianshapiro/velocity.git",
	"https://github.com/maxwellito/vivus.git",
	"https://github.com/peduarte/wallop.git",
	"https://github.com/imakewebthings/waypoints.git",
	"https://github.com/brownhci/WebGazer.git",
	"https://github.com/Popmotion/popmotion.git",
	"https://github.com/FezVrasta/popper.js.git",
	"https://github.com/kimmobrunfeldt/progressbar.js.git",
	"https://github.com/Nickersoft/push.js.git",
	"https://github.com/quilljs/quill.git",
	"https://github.com/davidmerfield/randomColor.git",
	"https://github.com/dixonandmoe/rellax.git",
	"https://github.com/bevacqua/rome.git",
	"https://github.com/terwanerik/ScrollTrigger.git",
	"https://github.com/themadcreator/seen.git",
	"https://github.com/lmgonzalves/segment.git",
	"https://github.com/HubSpot/select.git",
	"https://github.com/HubSpot/shepherd.git",
	"https://github.com/adobe-webplatform/Snap.svg.git",
	"https://github.com/tmort/Socialite.git",
	"https://github.com/RubaXa/Sortable.git",
	"https://github.com/fgnass/spin.js.git",
	"https://github.com/LunarLogic/starability.git",
	"https://github.com/t4t5/sweetalert.git",
	"https://github.com/limonte/sweetalert2.git",
	"https://github.com/nolimits4web/swiper.git",
	"https://github.com/sbstjn/timesheet.js.git",
	"https://github.com/robinparisi/tingle.git",
	"https://github.com/tinymce/tinymce.git",
	"https://github.com/frend/frend.co.git",
	"https://github.com/bfred-it/iphone-inline-video.git",
	"https://github.com/kaimallea/isMobile.git",
	"https://github.com/metafizzy/isotope.git",
	"https://github.com/callmecavs/jump.js.git",
	"https://github.com/JoelOtter/kajero.git",
	"https://github.com/madrobby/keymaster.git",
	"https://github.com/LPology/Simple-Ajax-Uploader.git",
	"https://github.com/aFarkas/lazysizes.git",
	"https://github.com/sparanoid/lightense-images.git",
	"https://github.com/javve/list.js.git",
	"https://github.com/meandmax/lory.git",
	"https://github.com/mahdif/loud-links.git",
	"https://github.com/julmot/mark.js.git",
	"https://github.com/yabwe/medium-editor.git",
	"https://github.com/asvd/microlight.git",
	"https://github.com/henriquea/minigrid.git",
	"https://github.com/legomushroom/mojs.git",
	"https://github.com/jacoborus/nanobar.git",
	"https://github.com/leongersen/noUiSlider.git",
	"https://github.com/wagerfield/parallax.git",
	"https://github.com/VincentGarreau/particles.js.git",
	"https://github.com/dimsemenov/PhotoSwipe.git",
	"https://github.com/dbushell/Pikaday.git",
	"https://github.com/pixijs/pixi.js.git",
	"https://github.com/algolia/places.git",
	"https://github.com/selz/plyr.git",
	"https://github.com/metafloor/bwip-js.git",
	"https://github.com/c3js/c3.git",
	"https://github.com/chartjs/Chart.js.git",
	"https://github.com/gionkunz/chartist-js.git",
	"https://github.com/nosir/cleave.js.git",
	"https://github.com/NeXTs/Clusterize.js.git",
	"https://github.com/GetmeUK/ContentTools.git",
	"https://github.com/dixso/custombox.git",
	"https://github.com/date-fns/date-fns.git",
	"https://github.com/frend/frend.co.git",
	"https://github.com/bevacqua/dragula.git",
	"https://github.com/imgix/drift.git",
	"https://github.com/enyo/dropzone.git",
	"https://github.com/michaelvillar/dynamics.js.git",
	"https://github.com/tholman/elevator.js.git",
	"https://github.com/ritz078/embed.js.git",
	"https://github.com/eqcss/eqcss.git",
	"https://github.com/kangax/fabric.js.git",
	"https://github.com/viljamis/feature.js.git",
	"https://github.com/chmln/flatpickr.git",
	"https://github.com/grafijs/grafi.git",
	"https://github.com/greensock/GreenSock-JS.git",
	"https://github.com/WickyNilliams/headroom.js.git",
	"https://github.com/Haroenv/holmes.git",
	"https://github.com/bevacqua/horsey.git",
	"https://github.com/bitshadow/iconate.git",
	"https://github.com/impress/impress.js.git",
	"https://github.com/tholman/intense-images.git",
	"https://github.com/juliangarnier/anime.git",
	"https://github.com/googlecreativelab/anypixel.git",
	"https://github.com/michalsnik/aos.git",
	"https://github.com/algolia/autocomplete.js.git",
	"https://github.com/LeaVerou/awesomplete.git",
	"https://github.com/kennethcachia/Background-Check.git",
	"https://github.com/feimosi/baguetteBox.js.git",
	"https://github.com/kazzkiq/balloon.css.git",
	"https://github.com/rishabhp/bideo.js.git",
	"https://github.com/dinbror/blazy.git",
	"https://github.com/tictail/bounce.js.git",
	"https://github.com/callmecavs/bricks.js.git",
}

func git_clone_url(url string, wg *sync.WaitGroup) {
	defer fmt.Println("(cloned)", url)
	cmd := exec.Command("git", "clone", "--depth", "1", url)
	cmd.Stdout = nil

	if err := cmd.Run(); err != nil {
		fmt.Println("(failed)", url)
	}
}

func main() {
	os.Setenv("GIT_TERMINAL_PROMPT", "0")

	var wg sync.WaitGroup

	for _, url := range Urls {
		wg.Add(1)
		go func(u string) {
			git_clone_url(u, &wg)
			wg.Done()
		}(url)
	}

	wg.Wait()
}
