ZCCO ; Save the Cache Object classes as XML files.
 ;---------------------------------------------------------------------------
 ; Copyright 2012 The Open Source Electronic Health Record Agent
 ;
 ; Licensed under the Apache License, Version 2.0 (the "License");
 ; you may not use this file except in compliance with the License.
 ; You may obtain a copy of the License at
 ;
 ;     http://www.apache.org/licenses/LICENSE-2.0
 ;
 ; Unless required by applicable law or agreed to in writing, software
 ; distributed under the License is distributed on an "AS IS" BASIS,
 ; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ; See the License for the specific language governing permissions and
 ; limitations under the License.
 ;---------------------------------------------------------------------------
 ;
 N  W "Capturing the Cache Object Classes"
 D ASKDIR Q:DIR["^"
 D SAVEALL(DIR)
 Q
SAVEALL(DIR)
 I '$$SLASH(DIR) Q
 D $SYSTEM.OBJ.GetClassList(.classes)
 W "Found these classes:",!,! ZW classes S X=""
 F  S X=$O(classes(X)) Q:X=""  D $SYSTEM.OBJ.Export(X_".cls",DIR_X_".xml")
 Q
ASKDIR
 R !,!,"Host output directory: ",DIR,! Q:DIR["^"   G:'$$SLASH(DIR) ASKDIR
 Q
SLASH(DIR)
 I $E(DIR,$L(DIR))?1(1"/",1"\") Q 1
 E  U $P W "Output directory must end in a slash!" Q 0