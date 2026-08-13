#STmodel_seasonal.mod
      
set comid_all :=
{'23989291'};

set comid_all_N within comid_all := {'23989291'};

set comid_all_P within comid_all := {};

set comid_N1 within comid_all_N := {'23989291'};

set comid_P1 within comid_all_P := {};

set seasons := 1..4 ordered; 

set years := 6..7 ordered;

set urban_land_uses := {'UIDU','URHD','URLD','URMD'};

set urban_soil_groups := {'A','B','C','D'};

set road_soil_groups := {'A','B','C','D'};


set urban_bmp :=
{'Extended_Dry_Detention_Basin','Grass_Swale_w_D','Green_Roof','Infiltration_Basin','Permeable_Pavement','Porous_Pavement_w_UD','Sand_Filter_w_UD','Wet_Pond'};

set urban_pervbmp within urban_bmp :=
{'Porous_Pavement_w_UD'};

set road_bmp :=
{'Extended_Dry_Detention_Basin','Grass_Swale_w_D','Infiltration_Basin','Infiltration_Chamber','Infiltration_Trench','Permeable_Pavement','Porous_Pavement_w_UD','Sand_Filter_w_UD','Wet_Pond'};

set ag_bmp :=
{'Conservation','Contour_Farming','Fert_75','Fert_90','Filterstrip','MIN_TILL','Terrace_Waterway','Terrace_Only','Waterway_Only','Grade_Stabilization','Diversion','Cover_Crops','Drainage_Management','Manure_Injection'};

set ag_tilebmp within ag_bmp :=
{'Drainage_Management'};

set septic_bmp :=
{'class_3_upgrade', 'class_4_upgrade', 'class_7_upgrade', 'class_8_upgrade', 'Sewer_convert'};

set graz_bmp :=
{'Offsite_Water'};

set ripbuf_bmp :=
{'Grassed_Buffer','Forested_Buffer'};

set point_bmp :=
{'C','C_F','FOURBDP_M','FOURBDP_M_C_F','MLE','MLE_C','C_M','M','SBR','SBR_C','SBR_C_F','SBR_DNF_C_F_M','SBR_DNF_M','FOURBDP_MBR_M','FOURBDP_MBR_M_C','MLE_MBR','MLE_MBR_C'};

set loads_all :=
{'point', 'urban', 'road', 'ag', 'septic', 'graz', 'storage', 'other'};
  
set area_sub :=
{'ag', 'graz'};
set urban_area_sub :=
{'urban'};
set road_area_sub :=
{'road'};

set transfer_types := {'coeff', 'notrib'};

set loads_riparian := 
 {'riparian'};

set limits := 
 {'limits'};

set total_load := 
 {'total_load'};

set other_loads := 
 {'other_load'};

set septic_bmp_totals := 
 {'total_parcels'};

param baseloads_N1 {comid_N1, seasons, years, loads_all} >= 0;
param riparianload_N1 {comid_N1, seasons, years, loads_riparian} >= 0;

param septic_bmp_implementationpotential {comid_all_N, septic_bmp} >= 0;
param septic_bmp_implementationtotal {comid_all_N, septic_bmp_totals} >= 0;

param area {comid_all, area_sub} >=0;
    
param urban_area {comid_all, urban_land_uses, urban_soil_groups, urban_area_sub} >=0;
param road_area {comid_all, road_soil_groups, road_area_sub} >=0;
param urban_bmp_implementationpotential {comid_all, urban_land_uses, urban_soil_groups, urban_bmp} >= 0;
param ag_bmp_implementationpotential {comid_all, ag_bmp} >= 0;
param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;

param ag_costs_capital {comid_all, ag_bmp};
param ag_costs_operations {comid_all, ag_bmp};
param graz_costs_capital {comid_all, graz_bmp};
param graz_costs_operations {comid_all, graz_bmp};
param point_costs_capital {comid_all, point_bmp};
param point_costs_operations {comid_all, point_bmp};
param urban_costs_capital {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};
param urban_costs_operations {comid_all, urban_land_uses, urban_soil_groups, urban_bmp};
param road_costs_capital {comid_all, road_soil_groups, road_bmp};
param road_costs_operations {comid_all, road_soil_groups, road_bmp};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};

param septic_costs_capital {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations {comid_all_N, septic_bmp} >= 0;



param ag_effic_N {comid_all_N, ag_bmp};
param graz_effic_N {comid_all_N, graz_bmp};
param point_effic_N {comid_all_N, point_bmp};
param urban_effic_N {comid_all_N, seasons, years, urban_land_uses, urban_soil_groups, urban_bmp};
param road_effic_N {comid_all_N, seasons, years, road_soil_groups, road_bmp};
param septic_effic_N {comid_all_N, septic_bmp};

param riparianremoval_N1 {c in comid_N1, s in seasons, y in years, b in ripbuf_bmp};
param loads_lim_N1 {seasons, years, limits} >= 0;


param transfer_coefficients_N1 {comid_N1, seasons, years, transfer_types};


param total_load_N1 {s in seasons, y in years, total_load} >= 0;


param agcost_frac >=0;
    
param agBMP_minarea;
param grazBMP_minarea;
param urbanBMP_minarea;
param roadBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;
param road_frac_min {road_bmp} >=0, <= 1;
param road_frac_max {r in road_bmp} >= road_frac_min[r], <= 1;
param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;
param graz_frac_min {graz_bmp} >= 0, <= 1;
param graz_frac_max {g in graz_bmp} >= graz_frac_min[g], <= 1;
param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {b in ripbuf_bmp} >= ripbuf_frac_min[b], <= 1;
  
param ps_coef {c in comid_all, p in point_bmp} := (point_costs_capital[c, p] + 
    point_costs_operations[c, p]);
param urban_coef {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} := urban_area[c,d,n,'urban'] * 
  (urban_costs_capital[c,d,n,u] + urban_costs_operations[c,d,n,u]) ;
param road_coef {c in comid_all, h in road_soil_groups, r in road_bmp} := road_area[c,h,'road'] * 
  (road_costs_capital[c,h,r] + road_costs_operations[c,h,r]) ;
param ag_coef {c in comid_all,  a in ag_bmp} := agcost_frac * area[c,'ag'] * 
  (ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;

param graz_coef {c in comid_all, g in graz_bmp} := agcost_frac * area[c,'graz'] * 
  (graz_costs_capital[c,g] + graz_costs_operations[c,g]) ;
param ripbuf_coef {c in comid_all, b in ripbuf_bmp} := 
  agcost_frac * (ripbuf_costs_capital[c, b] + ripbuf_costs_operations[c, b]);
  
var agBMP_bin {comid_all, ag_bmp} binary;
var grazBMP_bin {comid_all, graz_bmp} binary;
var urbanBMP_bin {comid_all, urban_bmp} binary;
var roadBMP_bin {comid_all, road_bmp} binary;

var point_dec {c in comid_all, p in point_bmp} binary;
# correct unbounded fraction variables - AI solution created NEOS error  
var urban_frac {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}
  >= 0, <= urban_frac_max[u] * urban_bmp_implementationpotential[c,d,n,u];
#  := 0; # default is 0 so not needed; 
#    >= urban_frac_min[u] * urban_bmp_implementationpotential[c,d,n,u],  
#    <= urban_frac_max[u] * urban_bmp_implementationpotential[c,d,n,u],
#    :=0;
var ag_frac {c in comid_all, a in ag_bmp}
   >= 0,
   <= ag_frac_max[a] * ag_bmp_implementationpotential[c,a];
#   := 0
#    >= ag_frac_min[a] * ag_bmp_implementationpotential[c, a],  
#    <= ag_frac_max[a] * ag_bmp_implementationpotential[c, a],
#    :=0;
var road_frac {c in comid_all, h in road_soil_groups, r in road_bmp}
  >= 0, 
  <= road_frac_max[r];
#  := 0;  
#   >= road_frac_min[r],
#   <= road_frac_max[r],
#   :=0;
var graz_frac {c in comid_all, g in graz_bmp}
  >= 0,
  <= graz_frac_max[g];
#  := 0;
#   >= graz_frac_min[g],
#   <= graz_frac_max[g],
#   :=0;
var ripbuf_length {c in comid_all, b in ripbuf_bmp} 
   >= 0, 
   <= unbuffered_banklength[c, b] * ripbuf_frac_max[b];
#   := 0;
   

   
# param septic_coef {c in comid_all, e in septic_bmp} :=
# ND substitute
param septic_coef {c in comid_all_N, e in septic_bmp} :=
   septic_costs_capital[c,e] + septic_costs_operations[c,e];
var septic_bin {comid_all_N, septic_bmp} binary;
var septic_frac {c in comid_all_N, e in septic_bmp} 
   >= 0, 
   <= septic_bmp_implementationpotential[c,e] := 0;
var inc_storage_load_N1{c in comid_N1, s in seasons, y in years} >= 0;
var no_storage_NPSload_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_ag_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_graz_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_urb_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_road_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_point_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_septic_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var reduced_storage_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

            var riparian_reduction_load_N1{c in comid_N1, s in seasons, y in years} >= 0;

minimize cost: sum {c in comid_all, p in point_bmp} (ps_coef[c,p] * point_dec[c,p]) +
sum {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} (urban_coef[c,d,n,u] * urban_frac[c,d,n,u]) +
sum {c in comid_all, h in road_soil_groups, r in road_bmp} (road_coef[c,h,r] * road_frac[c,h,r]) +
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, g in graz_bmp} (graz_coef[c,g] * graz_frac[c,g]) +
 

sum {c in comid_all, b in ripbuf_bmp} (ripbuf_coef[c, b] * ripbuf_length[c, b]) +
     sum {c in comid_all_N, e in septic_bmp} (septic_coef[c,e] * septic_frac[c,e]);
subject to calc_red_ag_load_N1{c in comid_N1, s in seasons, y in years}:

      reduced_ag_load_N1[c,s,y] =

      baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a]));

  
    subject to calc_red_graz_load_N1{c in comid_N1, s in seasons, y in years}:

      reduced_graz_load_N1[c,s,y] =

      baseloads_N1[c,s,y,'graz'] * (1 - sum {g in graz_bmp}(graz_effic_N[c,g] * graz_frac[c,g]));

  
    subject to calc_red_urb_load_N1{c in comid_N1, s in seasons, y in years}:

      reduced_urb_load_N1[c,s,y] =

      baseloads_N1[c,s,y,'urban'] * (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}(urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]));

  
    subject to calc_red_road_load_N1{c in comid_N1, s in seasons, y in years}:

      reduced_road_load_N1[c,s,y] =

      baseloads_N1[c,s,y,'road'] * (1 - sum{h in road_soil_groups, r in road_bmp}(road_effic_N[c,s,y,h,r] * road_frac[c,h,r]));
 
    
    subject to calc_red_point_load_N1{c in comid_N1, s in seasons, y in years}:

        reduced_point_load_N1[c,s,y] = 
 
        (baseloads_N1[c,s,y,'point'] + ((baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert']))  * (1 - sum{p in point_bmp}(point_effic_N[c,p] * point_dec[c,p]));

          
    subject to calc_red_septic_load_N1{c in comid_N1, s in seasons, y in years}:

        reduced_septic_load_N1[c,s,y] =
(baseloads_N1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e]);

          
    subject to calc_riparian_reduction_load_N1{c in comid_N1, s in seasons, y in years}:
          riparian_reduction_load_N1[c,s,y] =  
          sum {b in ripbuf_bmp}

        ripbuf_length[c,b] * riparianremoval_N1[c,s,y,b];
          #*

          # (
          #   reduced_ag_load_N1[c,s,y] / riparianload_N1[c,s,y,'riparian']

          #   +

          #     (1 - sum {d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}

          #      urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u])

          #   * (riparianload_N1[c,s,y,'riparian'] - baseloads_N1[c,s,y,'ag'])

          #   / riparianload_N1[c,s,y,'riparian']

          # );

  
      subject to calc_no_storage_NPSload_N1{c in comid_N1, s in seasons, y in years}:

          no_storage_NPSload_N1[c,s,y] =
          baseloads_N1[c,s,y,'other'] +
          reduced_ag_load_N1[c,s,y] + 
          reduced_urb_load_N1[c,s,y] +   
          reduced_road_load_N1[c,s,y] +
          reduced_point_load_N1[c,s,y] +
          reduced_septic_load_N1[c,s,y] -
          riparian_reduction_load_N1[c,s,y];
 
          
      subject to calc_red_storage_load_N1{c in comid_N1, s in seasons, y in years}:

      reduced_storage_load_N1[c,s,y] =

        if transfer_coefficients_N1[c,s,y,'notrib'] = 1 then
 
          (no_storage_NPSload_N1[c,s,y] * transfer_coefficients_N1[c,s,y,'coeff']) / (1 - transfer_coefficients_N1[c,s,y,'coeff'])

        else if transfer_coefficients_N1[c,s,y,'notrib'] = 0 & s = first(seasons) then

          if y = first(years) then
            baseloads_N1[c,s,y,'storage']

          else if y != first(years) then

            (no_storage_NPSload_N1[c,4,y-1] 
            * transfer_coefficients_N1[c,s,y,'coeff']) / (1 - transfer_coefficients_N1[c,s,y,'coeff'])

        else if transfer_coefficients_N1[c,s,y,'notrib'] = 0 & s != first(seasons) then

          (no_storage_NPSload_N1[c,s-1,y] 
* transfer_coefficients_N1[c,s,y,'coeff']) / (1 - transfer_coefficients_N1[c,s,y,'coeff']);

    
    subject to calc_inc_storage_load_N1{c in comid_N1, s in seasons, y in years}:

      inc_storage_load_N1[c,s,y] =

        reduced_storage_load_N1[c,s,y] + 
        reduced_ag_load_N1[c,s,y] + 
        reduced_urb_load_N1[c,s,y] +   
        reduced_road_load_N1[c,s,y] +
        reduced_point_load_N1[c,s,y] +
        reduced_septic_load_N1[c,s,y] -
        riparian_reduction_load_N1[c,s,y];

    
subject to total_loads_N1{s in seasons, y in years: y = 7}:
sum{c in comid_N1} (inc_storage_load_N1[c,s,y]) <= loads_lim_N1[s,7,'limits'];

subject to riparian_loads_N1 {c in comid_N1, s in seasons, y in years}:
sum {b in ripbuf_bmp} ripbuf_length[c, b] * riparianremoval_N1[c,s,y,b] <= 
 
            (baseloads_N1[c,s,y,'ag'] 
 
            * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) 

            + (1 - sum{d in urban_land_uses, n in urban_soil_groups, u in urban_bmp} 

              (urban_effic_N[c,s,y,d,n,u] * urban_frac[c,d,n,u]))
 
            * (riparianload_N1[c,s,y,'riparian'] - baseloads_N1[c,s,y,'ag']));

subject to urban_frac_min2 {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] >= urban_frac_min[u] * urban_bmp_implementationpotential[c,d,n,u] * urbanBMP_bin[c,u];
    subject to ag_frac_min2 {c in comid_all, a in ag_bmp}:
ag_frac[c,a] >= ag_frac_min[a] * ag_bmp_implementationpotential[c,a] * agBMP_bin[c,a];
    subject to graz_frac_min2{c in comid_all,g in graz_bmp}:
graz_frac[c,g] >= graz_frac_min[g] * grazBMP_bin[c,g];

#    subject to road_frac_min2{c in comid_all, h in road_soil_groups,r in road_bmp}:
# road_frac[c,h,r] >= road_frac_min[r] * roadBMP_bin[c,r];

# ND replacement
  subject to road_frac_min2 {c in comid_all, r in road_bmp}:
sum {h in road_soil_groups} road_frac[c,h,r] >= road_frac_min[r] * roadBMP_bin[c,r];

# ND added Aggregate min area per BMP (recommended) 
  subject to road_treat_min {c in comid_all, r in road_bmp}:
sum {h in road_soil_groups} road_frac[c,h,r] * road_area[c,h,'road'] >= roadBMP_minarea * roadBMP_bin[c,r];
    
subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];

subject to graz_treat_min {c in comid_all, g in graz_bmp}:
graz_frac[c,g] * area[c,'graz'] >= grazBMP_minarea * grazBMP_bin[c,g];

subject to urban_treat_min {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] * urban_area[c,d,n,'urban'] >= urbanBMP_minarea * urbanBMP_bin[c,u];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];

subject to graz_frac_const {c in comid_all, g in graz_bmp}:
graz_frac[c,g] <= grazBMP_bin[c,g];
  
subject to urban_frac_const {c in comid_all, d in urban_land_uses, n in urban_soil_groups, u in urban_bmp}:
urban_frac[c,d,n,u] <= urbanBMP_bin[c,u];

#ND added missing 6/3/26;
subject to road_frac_const {c in comid_all, h in road_soil_groups, r in road_bmp}:
    road_frac[c,h,r] <= roadBMP_bin[c,r];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to graz_frac_limit {c in comid_all}: 
sum {g in graz_bmp} graz_frac[c,g] <= 1;
# prevents multiple BMPs being implemented on the same area

#subject to urban_frac_limit {c in comid_all}: 
#sum {u in urban_bmp, d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,u] <= 1;
# prevents multiple BMPs being implemented on the same area

# ND substitute - drop
# Per-subarea (d,n) constraint
 subject to urban_frac_limit2 {c in comid_all, d in urban_land_uses, n in urban_soil_groups}:
  sum {u in urban_bmp} urban_frac[c,d,n,u] <= 1;

subject to road_frac_limit {c in comid_all}: 
sum {r in road_bmp, h in road_soil_groups} road_frac[c,h,r] <= 1;
# prevents multiple BMPs being implemented on the same area

# ND substitute - try dropping this because not updated during bootstraps
# Per-soil-group constraint
# subject to road_frac_limit2 {c in comid_all, h in road_soil_groups}:
#  sum {r in road_bmp} road_frac[c,h,r] <= 1;
# prevents multiple BMPs being implemented on the same sub-area

subject to point_dec_limit {c in comid_all}: 
sum {p in point_bmp} point_dec[c,p] <= 1;
# limits each WWTP to one upgrade

subject to total_banks {c in comid_all}:
sum {b in ripbuf_bmp} ripbuf_length[c,b] <=  
   unbuffered_banklength[c,'Forested_Buffer']; #ND 7/8/2026 changed from max
  


subject to roads_and_parkinglots {c in comid_all}:
sum{up in urban_pervbmp, d in urban_land_uses, n in urban_soil_groups} urban_frac[c,d,n,up] <= max {up in urban_pervbmp, d in urban_land_uses, n in urban_soil_groups} 
   urban_bmp_implementationpotential[c,d,n,up];


subject to tiledrains {c in comid_all}:
sum{agti in ag_tilebmp} ag_frac[c, agti] <= max {agti in ag_tilebmp}
   ag_bmp_implementationpotential[c,agti];

subject to septic_reduction_limit_N1 {c in comid_N1, s in seasons, y in years}:
sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e] <= baseloads_N1[c,s,y,'septic'];

