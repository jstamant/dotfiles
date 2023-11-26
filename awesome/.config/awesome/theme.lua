local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- TODO can use GTK theme elements with beautiful.gtk

-- TODO eliminate as much of default theme assets as possible!
local gears = require("gears")
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()
local wibox = require("wibox")


local theme = {}

theme.font = "DejaVu Mono Sans 10"

local xcolors = xresources.get_current_theme()
theme.colors = {}
theme.colors.background     = xcolors.color0
theme.colors.background_alt = xcolors.color8
theme.colors.foreground     = xcolors.color7
theme.colors.foreground_alt = xcolors.color15
theme.colors.primary        = xcolors.color4 -- Blue
theme.colors.secondary      = xcolors.color1 -- Red
theme.colors.alert          = theme.colors.secondary

theme.fg_focus              = theme.colors.foreground
theme.fg_normal             = theme.colors.foreground
theme.fg_urgent             = "#ffffff"
theme.fg_minimize           = "#ffffff"

theme.bg_focus              = theme.colors.background_alt
theme.bg_normal             = theme.colors.background
theme.bg_urgent             = theme.colors.alert
theme.bg_minimize           = "#444444"

theme.useless_gap           = dpi(0)
theme.border_width          = dpi(1)
theme.border_normal         = "#000000"
theme.border_focus          = "#535d6c"
theme.border_marked         = "#91231c"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
local taglist_square_size = dpi(4)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

-- TODO change this to use the wallpapers at .local/share/wallpapers
-- theme.wallpaper = themes_path.."default/background.png"
theme.wallpaper = gfs.get_xdg_config_home() .. "awesome/wall.jpg"


-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Tasklist theme settings
-- theme.tasklist_fg_normal = nil
theme.tasklist_bg_normal = theme.colors.background
-- theme.tasklist_fg_focus = nil
theme.tasklist_bg_focus = theme.tasklist_bg_normal
-- theme.tasklist_fg_urgent = nil
-- theme.tasklist_bg_urgent = nil
-- theme.tasklist_fg_minimize = nil
-- theme.tasklist_bg_minimize = nil
-- theme.tasklist_bg_image_normal = nil
-- theme.tasklist_bg_image_focus = nil
-- theme.tasklist_bg_image_urgent = nil
-- theme.tasklist_bg_image_minimize = nil
-- theme.tasklist_disable_icon = nil
-- theme.tasklist_disable_task_name = nil
-- theme.tasklist_plain_task_name = nil
-- theme.tasklist_font = nil
-- theme.tasklist_align = nil
-- theme.tasklist_font_focus = nil
-- theme.tasklist_font_minimized = nil
-- theme.tasklist_font_urgent = nil
-- theme.tasklist_spacing = nil
-- theme.tasklist_shape = nil
-- theme.tasklist_shape_border_width = nil
-- theme.tasklist_shape_border_color = nil
-- theme.tasklist_shape_focus = nil
-- theme.tasklist_shape_border_width_focus = nil
-- theme.tasklist_shape_border_color_focus = nil
-- theme.tasklist_shape_minimized = nil
-- theme.tasklist_shape_border_width_minimized = nil
-- theme.tasklist_shape_border_color_minimized = nil
-- theme.tasklist_shape_urgent = nil
-- theme.tasklist_shape_border_width_urgent = nil
-- theme.tasklist_shape_border_color_urgent = nil

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)
-- theme.awesome_icon = wibox.widget.imagebox()
-- theme.awesome_icon:set_image(gfs.get_xdg_config_home() .. "awesome/arch_20.xpm")

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil


-- Default variables
-- theme.font = nil
-- theme.wallpaper = nil
-- theme.font = nil

-- arcchart
-- theme.arcchart_border_color = nil
-- theme.arcchart_color = nil
-- theme.arcchart_border_width = nil
-- theme.arcchart_paddings = nil
-- theme.arcchart_thickness = nil

-- awesome
-- theme.awesome_icon = nil

-- border
-- theme.border_marked = nil
-- theme.border_width = nil
-- theme.border_normal = nil
-- theme.border_focus = nil
-- theme.border_marked = nil
-- theme.border_focus = nil
-- theme.border_normal = nil
-- theme.border_width = nil

-- calendar
-- theme.calendar_style = nil
-- theme.calendar_font = nil
-- theme.calendar_spacing = nil
-- theme.calendar_week_numbers = nil
-- theme.calendar_start_sunday = nil
-- theme.calendar_long_weekdays = nil

-- checkbox
-- theme.checkbox_border_width = nil
-- theme.checkbox_bg = nil
-- theme.checkbox_border_color = nil
-- theme.checkbox_check_border_color = nil
-- theme.checkbox_check_border_width = nil
-- theme.checkbox_check_color = nil
-- theme.checkbox_shape = nil
-- theme.checkbox_check_shape = nil
-- theme.checkbox_paddings = nil
-- theme.checkbox_color = nil

-- column
-- theme.column_count = nil

-- cursor
-- theme.cursor_mouse_resize = nil
-- theme.cursor_mouse_move = nil

-- enable
-- theme.enable_spawn_cursor = nil

-- fullscreen
-- theme.fullscreen_hide_border = nil

-- gap
-- theme.gap_single_client = nil

-- graph
-- theme.graph_bg = nil
-- theme.graph_fg = nil
-- theme.graph_border_color = nil

-- hotkeys
-- theme.hotkeys_bg = nil
-- theme.hotkeys_fg = nil
-- theme.hotkeys_border_width = nil
-- theme.hotkeys_border_color = nil
-- theme.hotkeys_shape = nil
-- theme.hotkeys_modifiers_fg = nil
-- theme.hotkeys_label_bg = nil
-- theme.hotkeys_label_fg = nil
-- theme.hotkeys_font = nil
-- theme.hotkeys_description_font = nil
-- theme.hotkeys_group_margin = nil

-- icon
-- theme.icon_theme = nil

-- layout
-- theme.layout_cornernw = nil
-- theme.layout_cornerne = nil
-- theme.layout_cornersw = nil
-- theme.layout_cornerse = nil
-- theme.layout_fairh = nil
-- theme.layout_fairv = nil
-- theme.layout_floating = nil
-- theme.layout_magnifier = nil
-- theme.layout_max = nil
-- theme.layout_fullscreen = nil
-- theme.layout_spiral = nil
-- theme.layout_dwindle = nil
-- theme.layout_tile = nil
-- theme.layout_tiletop = nil
-- theme.layout_tilebottom = nil
-- theme.layout_tileleft = nil

-- layoutlist
-- theme.layoutlist_fg_normal = nil
-- theme.layoutlist_bg_normal = nil
-- theme.layoutlist_fg_selected = nil
-- theme.layoutlist_bg_selected = nil
-- theme.layoutlist_disable_icon = nil
-- theme.layoutlist_disable_name = nil
-- theme.layoutlist_font = nil
-- theme.layoutlist_align = nil
-- theme.layoutlist_font_selected = nil
-- theme.layoutlist_spacing = nil
-- theme.layoutlist_shape = nil
-- theme.layoutlist_shape_border_width = nil
-- theme.layoutlist_shape_border_color = nil
-- theme.layoutlist_shape_selected = nil
-- theme.layoutlist_shape_border_width_selected = nil
-- theme.layoutlist_shape_border_color_selected = nil

-- master
-- theme.master_width_factor = nil
-- theme.master_fill_policy = nil
-- theme.master_count = nil

-- maximized
-- theme.maximized_honor_padding = nil
-- theme.maximized_hide_border = nil

-- menu
-- theme.menu_submenu_icon = nil
-- theme.menu_font = nil
-- theme.menu_height = nil
-- theme.menu_width = nil
-- theme.menu_border_color = nil
-- theme.menu_border_width = nil
-- theme.menu_fg_focus = nil
-- theme.menu_bg_focus = nil
-- theme.menu_fg_normal = nil
-- theme.menu_bg_normal = nil
-- theme.menu_submenu = nil

-- menubar
-- theme.menubar_fg_normal = nil
-- theme.menubar_bg_normal = nil
-- theme.menubar_border_width = nil
-- theme.menubar_border_color = nil
-- theme.menubar_fg_normal = nil
-- theme.menubar_bg_normal = nil

-- notification
-- theme.notification_font = nil
-- theme.notification_bg = nil
-- theme.notification_fg = nil
-- theme.notification_border_width = nil
-- theme.notification_border_color = nil
-- theme.notification_shape = nil
-- theme.notification_opacity = nil
-- theme.notification_margin = nil
theme.notification_width = 500
theme.notification_height = 100
-- theme.notification_max_width = nil
-- theme.notification_max_height = nil
-- theme.notification_icon_size = nil

-- piechart
-- theme.piechart_border_color = nil
-- theme.piechart_border_width = nil
-- theme.piechart_colors = nil

-- progressbar
-- theme.progressbar_bg = nil
-- theme.progressbar_fg = nil
-- theme.progressbar_shape = nil
-- theme.progressbar_border_color = nil
-- theme.progressbar_border_width = nil
-- theme.progressbar_bar_shape = nil
-- theme.progressbar_bar_border_width = nil
-- theme.progressbar_bar_border_color = nil
-- theme.progressbar_margins = nil
-- theme.progressbar_paddings = nil

-- prompt
-- theme.prompt_fg_cursor = nil
-- theme.prompt_bg_cursor = nil
-- theme.prompt_font = nil
-- theme.prompt_fg = nil
-- theme.prompt_bg = nil

-- radialprogressbar
-- theme.radialprogressbar_border_color = nil
-- theme.radialprogressbar_color = nil
-- theme.radialprogressbar_border_width = nil
-- theme.radialprogressbar_paddings = nil

-- separator
-- theme.separator_thickness = nil
-- theme.separator_border_color = nil
-- theme.separator_border_width = nil
-- theme.separator_span_ratio = nil
-- theme.separator_color = nil
-- theme.separator_shape = nil

-- slider
-- theme.slider_bar_border_width = nil
-- theme.slider_bar_border_color = nil
-- theme.slider_handle_border_color = nil
-- theme.slider_handle_border_width = nil
-- theme.slider_handle_width = nil
-- theme.slider_handle_color = nil
-- theme.slider_handle_shape = nil
-- theme.slider_bar_shape = nil
-- theme.slider_bar_height = nil
-- theme.slider_bar_margins = nil
-- theme.slider_handle_margins = nil
-- theme.slider_bar_color = nil

-- snap
-- theme.snap_bg = nil
-- theme.snap_border_width = nil
-- theme.snap_shape = nil

-- snapper
-- theme.snapper_gap = nil

-- Systray settings
theme.systray_icon_spacing = 0
theme.bg_systray = theme.bg_normal

-- taglist
-- theme.taglist_fg_focus = nil
-- theme.taglist_bg_focus = nil
-- theme.taglist_fg_urgent = nil
-- theme.taglist_bg_urgent = nil
-- theme.taglist_bg_occupied = nil
-- theme.taglist_fg_occupied = nil
-- theme.taglist_bg_empty = nil
-- theme.taglist_fg_empty = nil
-- theme.taglist_bg_volatile = nil
-- theme.taglist_fg_volatile = nil
-- theme.taglist_squares_sel = nil
-- theme.taglist_squares_unsel = nil
-- theme.taglist_squares_sel_empty = nil
-- theme.taglist_squares_unsel_empty = nil
-- theme.taglist_squares_resize = nil
-- theme.taglist_disable_icon = nil
-- theme.taglist_font = nil
-- theme.taglist_spacing = nil
-- theme.taglist_shape = gears.shape.circle
-- theme.taglist_shape_border_width = nil
-- theme.taglist_shape_border_color = nil
-- theme.taglist_shape_empty = nil
-- theme.taglist_shape_border_width_empty = nil
-- theme.taglist_shape_border_color_empty = nil
-- theme.taglist_shape_focus = nil
-- theme.taglist_shape_border_width_focus = nil
-- theme.taglist_shape_border_color_focus = nil
-- theme.taglist_shape_urgent = nil
-- theme.taglist_shape_border_width_urgent = nil
-- theme.taglist_shape_border_color_urgent = nil
-- theme.taglist_shape_volatile = nil
-- theme.taglist_shape_border_width_volatile = nil
-- theme.taglist_shape_border_color_volatile = nil

-- tasklist
-- theme.tasklist_fg_normal = nil
-- theme.tasklist_bg_normal = nil
-- theme.tasklist_fg_focus = nil
-- theme.tasklist_bg_focus = nil
-- theme.tasklist_fg_urgent = nil
-- theme.tasklist_bg_urgent = nil
-- theme.tasklist_fg_minimize = nil
-- theme.tasklist_bg_minimize = nil
-- theme.tasklist_bg_image_normal = nil
-- theme.tasklist_bg_image_focus = nil
-- theme.tasklist_bg_image_urgent = nil
-- theme.tasklist_bg_image_minimize = nil
-- theme.tasklist_disable_icon = nil
-- theme.tasklist_disable_task_name = nil
-- theme.tasklist_plain_task_name = nil
-- theme.tasklist_font = nil
-- theme.tasklist_align = nil
-- theme.tasklist_font_focus = nil
-- theme.tasklist_font_minimized = nil
-- theme.tasklist_font_urgent = nil
-- theme.tasklist_spacing = nil
-- theme.tasklist_shape = nil
-- theme.tasklist_shape_border_width = nil
-- theme.tasklist_shape_border_color = nil
-- theme.tasklist_shape_focus = nil
-- theme.tasklist_shape_border_width_focus = nil
-- theme.tasklist_shape_border_color_focus = nil
-- theme.tasklist_shape_minimized = nil
-- theme.tasklist_shape_border_width_minimized = nil
-- theme.tasklist_shape_border_color_minimized = nil
-- theme.tasklist_shape_urgent = nil
-- theme.tasklist_shape_border_width_urgent = nil
-- theme.tasklist_shape_border_color_urgent = nil

-- titlebar
-- theme.titlebar_fg_normal = nil
-- theme.titlebar_bg_normal = nil
-- theme.titlebar_bgimage_normal = nil
-- theme.titlebar_fg = nil
-- theme.titlebar_bg = nil
-- theme.titlebar_bgimage = nil
-- theme.titlebar_fg_focus = nil
-- theme.titlebar_bg_focus = nil
-- theme.titlebar_bgimage_focus = nil
-- theme.titlebar_floating_button_normal = nil
-- theme.titlebar_maximized_button_normal = nil
-- theme.titlebar_minimize_button_normal = nil
-- theme.titlebar_minimize_button_normal_hover = nil
-- theme.titlebar_minimize_button_normal_press = nil
-- theme.titlebar_close_button_normal = nil
-- theme.titlebar_close_button_normal_hover = nil
-- theme.titlebar_close_button_normal_press = nil
-- theme.titlebar_ontop_button_normal = nil
-- theme.titlebar_sticky_button_normal = nil
-- theme.titlebar_floating_button_focus = nil
-- theme.titlebar_maximized_button_focus = nil
-- theme.titlebar_minimize_button_focus = nil
-- theme.titlebar_minimize_button_focus_hover = nil
-- theme.titlebar_minimize_button_focus_press = nil
-- theme.titlebar_close_button_focus = nil
-- theme.titlebar_close_button_focus_hover = nil
-- theme.titlebar_close_button_focus_press = nil
-- theme.titlebar_ontop_button_focus = nil
-- theme.titlebar_sticky_button_focus = nil
-- theme.titlebar_floating_button_normal_active = nil
-- theme.titlebar_floating_button_normal_active_hover = nil
-- theme.titlebar_floating_button_normal_active_press = nil
-- theme.titlebar_maximized_button_normal_active = nil
-- theme.titlebar_maximized_button_normal_active_hover = nil
-- theme.titlebar_maximized_button_normal_active_press = nil
-- theme.titlebar_ontop_button_normal_active = nil
-- theme.titlebar_ontop_button_normal_active_hover = nil
-- theme.titlebar_ontop_button_normal_active_press = nil
-- theme.titlebar_sticky_button_normal_active = nil
-- theme.titlebar_sticky_button_normal_active_hover = nil
-- theme.titlebar_sticky_button_normal_active_press = nil
-- theme.titlebar_floating_button_focus_active = nil
-- theme.titlebar_floating_button_focus_active_hover = nil
-- theme.titlebar_floating_button_focus_active_press = nil
-- theme.titlebar_maximized_button_focus_active = nil
-- theme.titlebar_maximized_button_focus_active_hover = nil
-- theme.titlebar_maximized_button_focus_active_press = nil
-- theme.titlebar_ontop_button_focus_active = nil
-- theme.titlebar_ontop_button_focus_active_hover = nil
-- theme.titlebar_ontop_button_focus_active_press = nil
-- theme.titlebar_sticky_button_focus_active = nil
-- theme.titlebar_sticky_button_focus_active_hover = nil
-- theme.titlebar_sticky_button_focus_active_press = nil
-- theme.titlebar_floating_button_normal_inactive = nil
-- theme.titlebar_floating_button_normal_inactive_hover = nil
-- theme.titlebar_floating_button_normal_inactive_press = nil
-- theme.titlebar_maximized_button_normal_inactive = nil
-- theme.titlebar_maximized_button_normal_inactive_hover = nil
-- theme.titlebar_maximized_button_normal_inactive_press = nil
-- theme.titlebar_ontop_button_normal_inactive = nil
-- theme.titlebar_ontop_button_normal_inactive_hover = nil
-- theme.titlebar_ontop_button_normal_inactive_press = nil
-- theme.titlebar_sticky_button_normal_inactive = nil
-- theme.titlebar_sticky_button_normal_inactive_hover = nil
-- theme.titlebar_sticky_button_normal_inactive_press = nil
-- theme.titlebar_floating_button_focus_inactive = nil
-- theme.titlebar_floating_button_focus_inactive_hover = nil
-- theme.titlebar_floating_button_focus_inactive_press = nil
-- theme.titlebar_maximized_button_focus_inactive = nil
-- theme.titlebar_maximized_button_focus_inactive_hover = nil
-- theme.titlebar_maximized_button_focus_inactive_press = nil
-- theme.titlebar_ontop_button_focus_inactive = nil
-- theme.titlebar_ontop_button_focus_inactive_hover = nil
-- theme.titlebar_ontop_button_focus_inactive_press = nil
-- theme.titlebar_sticky_button_focus_inactive = nil
-- theme.titlebar_sticky_button_focus_inactive_hover = nil
-- theme.titlebar_sticky_button_focus_inactive_press = nil

-- tooltip
-- theme.tooltip_border_color = nil
-- theme.tooltip_bg = nil
-- theme.tooltip_fg = nil
-- theme.tooltip_font = nil
-- theme.tooltip_border_width = nil
-- theme.tooltip_opacity = nil
-- theme.tooltip_shape = nil
-- theme.tooltip_align = nil

-- useless
-- theme.useless_gap = nil
-- theme.useless_gap = nil

-- wibar
-- theme.wibar_stretch = nil
-- theme.wibar_border_width = nil
-- theme.wibar_border_color = nil
-- theme.wibar_ontop = nil
-- theme.wibar_cursor = nil
-- theme.wibar_opacity = nil
-- theme.wibar_type = nil
-- theme.wibar_width = nil
theme.wibar_height = 30
-- theme.wibar_bg = nil
-- theme.wibar_bgimage = nil
-- theme.wibar_fg = nil
-- theme.wibar_shape = nil





-- theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/powerarrow-dark"
-- theme.wallpaper                                 = theme.dir .. "/wall.png"
-- theme.font                                      = "DejaVu Sans Mono 10"
-- theme.fg_normal                                 = "#DDDDFF"
-- theme.fg_focus                                  = "#EA6F81"
-- theme.fg_urgent                                 = "#CC9393"
-- theme.bg_normal                                 = "#1A1A1A"
-- theme.bg_focus                                  = "#313131"
-- theme.bg_urgent                                 = "#1A1A1A"
-- theme.border_width                              = dpi(1)
-- theme.border_normal                             = "#3F3F3F"
-- theme.border_focus                              = "#7F7F7F"
-- theme.border_marked                             = "#CC9393"
-- theme.tasklist_bg_focus                         = "#1A1A1A"
-- theme.titlebar_bg_focus                         = theme.bg_focus
-- theme.titlebar_bg_normal                        = theme.bg_normal
-- theme.titlebar_fg_focus                         = theme.fg_focus
-- theme.menu_height                               = dpi(16)
-- theme.menu_width                                = dpi(140)
-- theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
-- theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
-- theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
-- theme.layout_tile                               = theme.dir .. "/icons/tile.png"
-- theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
-- theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
-- theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
-- theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
-- theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
-- theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
-- theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
-- theme.layout_max                                = theme.dir .. "/icons/max.png"
-- theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
-- theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
-- theme.layout_floating                           = theme.dir .. "/icons/floating.png"
-- theme.widget_ac                                 = theme.dir .. "/icons/ac.png"
-- theme.widget_battery                            = theme.dir .. "/icons/battery.png"
-- theme.widget_battery_low                        = theme.dir .. "/icons/battery_low.png"
-- theme.widget_battery_empty                      = theme.dir .. "/icons/battery_empty.png"
-- theme.widget_mem                                = theme.dir .. "/icons/mem.png"
-- theme.widget_cpu                                = theme.dir .. "/icons/cpu.png"
-- theme.widget_temp                               = theme.dir .. "/icons/temp.png"
-- theme.widget_net                                = theme.dir .. "/icons/net.png"
-- theme.widget_hdd                                = theme.dir .. "/icons/hdd.png"
-- theme.widget_music                              = theme.dir .. "/icons/note.png"
-- theme.widget_music_on                           = theme.dir .. "/icons/note_on.png"
-- theme.widget_vol                                = theme.dir .. "/icons/vol.png"
-- theme.widget_vol_low                            = theme.dir .. "/icons/vol_low.png"
-- theme.widget_vol_no                             = theme.dir .. "/icons/vol_no.png"
-- theme.widget_vol_mute                           = theme.dir .. "/icons/vol_mute.png"
-- theme.widget_mail                               = theme.dir .. "/icons/mail.png"
-- theme.widget_mail_on                            = theme.dir .. "/icons/mail_on.png"
-- theme.tasklist_plain_task_name                  = true
-- theme.tasklist_disable_icon                     = true
-- theme.useless_gap                               = dpi(0)
-- theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"
-- theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
-- theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
-- theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
-- theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
-- theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
-- theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
-- theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
-- theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
-- theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
-- theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"
-- theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
-- theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
-- theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
-- theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
-- theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
-- theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
-- theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"

return theme
