#include <MincHask.h>
  
module MincHask.Types where

import Foreign.C.Types 
import Foreign.Ptr

{# context lib="minc2" #}

data VolProps
data DimStruct
data VolStruct
data VolumeList

{# pointer *mivolprops as VolumeProps -> VolProps #}
{# pointer *mivolume as Volume -> VolStruct  #}
{# pointer *midimension as Dimension #} -- -> DimStruct #}
{# pointer *midimhandle_t as DimArray -> Dimension #}
--{# pointer *void as VolumeListPtr -> VolumeList #}

{#enum mitype_t as Type {underscoreToCase} #}
{#enum miclass_t as Class {underscoreToCase} #}
{#enum midimclass_t as DimClass {underscoreToCase} #}
{#enum miorder_t as DimOrder {underscoreToCase} #}
{#enum mivoxel_order_t as VoxOrder {underscoreToCase} #}
{#enum miflipping_t as VoxFlipping {underscoreToCase} #}
{#enum micompression_t as Compression {underscoreToCase} #}

type MiBoolean = {#type miboolean_t #}
type MiDimattr = {#type midimattr_t #}
type MiSize = {#type misize_t #}
type HSize = {#type misize_t #}

data MiSComplex = MiSComplex CShort CShort 
{#pointer *miscomplex_t as MiSComplexPtr -> MiSComplex #}

data MiIComplex = MiIComplex CInt CInt
{#pointer *miicomplex_t as MiIComplexPtr -> MiIComplex #}

data MiDComplex = MiDComplex CDouble CDouble
{#pointer *midcomplex_t as MiDComplexPtr -> MiDComplex #}
