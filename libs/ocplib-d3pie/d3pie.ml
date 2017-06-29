(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open D3pie_types

type d3pie_title = {
  text : string;
}

let create_title text =
  let title : title Js.t = Js.Unsafe.obj [| |] in
  title##text <- text;
  title

let class_of_title title =
  create_title (Js.string title.text)

let default_title = {
  text = "";
}

type d3pie_header = {
  title : d3pie_title;
}

let create_header title =
  let header : header Js.t = Js.Unsafe.obj [| |] in
  header##title <- title;
  header

let class_of_header header =
  create_header (class_of_title header.title)

let default_header = {
  title = default_title;
}
	       
type d3pie_data_content = {
  label : string;
  value : int;
}

let create_data_content label value =
  let data_content : dataContent Js.t = Js.Unsafe.obj [||] in
  data_content##label <- label;
  data_content##value <- value;
  data_content

let class_of_data_content data_content =
  create_data_content (Js.string data_content.label) data_content.value
		      
type d3pie_data = {
  content : d3pie_data_content list;
}

let create_data content =
  let data : data Js.t = Js.Unsafe.obj [||] in
  data##content <- content;
  data

let class_of_data data =
  let content = List.map class_of_data_content data.content in
  create_data (Js.array (Array.of_list content))

let default_data = {
  content = [];
}
		     
type d3pie_settings = {
  data : d3pie_data;
  header : d3pie_header;
}

let create_settings data header =
  let settings : settings Js.t = Js.Unsafe.obj [||] in
  settings##data <- data;
  settings##header <- header;
  settings

let class_of_settings settings =
  create_settings
    (class_of_data settings.data)
    (class_of_header settings.header)

let default_settings = {
  data = default_data;
  header = default_header;
}

let update_data_content data_content settings =
  {
    settings with
    data = {
      (* settings.data with *)
      content = data_content; 
    };
  }
			 
let d3pie div settings =
  let d3pie_cstr = Js.Unsafe.global##_d3pie in
  Js.Unsafe.new_obj d3pie_cstr [|
    Js.Unsafe.inject div;
    Js.Unsafe.inject (class_of_settings settings);
  |]
