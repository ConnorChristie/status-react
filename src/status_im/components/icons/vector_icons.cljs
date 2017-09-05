(ns status-im.components.icons.vector-icons
  (:require-macros [status-im.utils.slurp :refer [slurp-svg]])
  (:require [reagent.core :as r]
            [status-im.utils.platform :refer [ios?]]
            [status-im.components.styles :as st]
            [status-im.components.react :as rn]
            [status-im.react-native.js-dependencies :as rn-dependencies]))

(defn get-property [name]
  (aget rn-dependencies/svg name))

(defn adapt-class [class]
  (when class
    (r/adapt-react-class class)))

(defn get-class [name]
  (adapt-class (get-property name)))

(def svg (get-class "Svg"))
(def g (get-class "G"))
(def rect (get-class "Rect"))
(def path (get-class "Path"))
(def use-def (get-class "Use"))
(def defs (get-class "Defs"))

(def icons {:icons/chats               (slurp-svg "./resources/icons/bottom/chats_gray.svg")
            :icons/chats-active        (slurp-svg "./resources/icons/bottom/chats_active.svg")
            :icons/contacts            (slurp-svg "./resources/icons/bottom/contacts_gray.svg")
            :icons/contacts-active     (slurp-svg "./resources/icons/bottom/contacts_active.svg")
            :icons/discover            (slurp-svg "./resources/icons/bottom/discover_gray.svg")
            :icons/discover-active     (slurp-svg "./resources/icons/bottom/discover_active.svg")
            :icons/wallet              (slurp-svg "./resources/icons/bottom/wallet_gray.svg")
            :icons/wallet-active       (slurp-svg "./resources/icons/bottom/wallet_active.svg")
            :icons/speaker             (slurp-svg "./resources/icons/speaker.svg")
            :icons/speaker-off         (slurp-svg "./resources/icons/speaker_off.svg")
            :icons/transaction-history (slurp-svg "./resources/icons/transaction_history.svg")
            :icons/add                 (slurp-svg "./resources/icons/add.svg")
            :icons/add-wallet          (slurp-svg "./resources/icons/add_wallet.svg")
            :icons/address             (slurp-svg "./resources/icons/address.svg")
            :icons/arrow-left          (slurp-svg "./resources/icons/arrow_left.svg")
            :icons/arrow-right         (slurp-svg "./resources/icons/arrow_right.svg")
            :icons/attach              (slurp-svg "./resources/icons/attach.svg")
            :icons/back                (slurp-svg "./resources/icons/back.svg")
            :icons/browse              (slurp-svg "./resources/icons/browse.svg")
            :icons/close               (slurp-svg "./resources/icons/close.svg")
            :icons/dots-horizontal     (slurp-svg "./resources/icons/dots_horizontal.svg")
            :icons/dots-vertical       (slurp-svg "./resources/icons/dots_vertical.svg")
            :icons/exclamation_mark    (slurp-svg "./resources/icons/exclamation_mark.svg")
            :icons/filter              (slurp-svg "./resources/icons/filter.svg")
            :icons/forward             (slurp-svg "./resources/icons/forward.svg")
            :icons/fullscreen          (slurp-svg "./resources/icons/fullscreen.svg")
            :icons/group-big           (slurp-svg "./resources/icons/group_big.svg")
            :icons/group-chat          (slurp-svg "./resources/icons/group_chat.svg")
            :icons/hamburger           (slurp-svg "./resources/icons/hamburger.svg")
            :icons/hidden              (slurp-svg "./resources/icons/hidden.svg")
            :icons/mic                 (slurp-svg "./resources/icons/mic.svg")
            :icons/ok                  (slurp-svg "./resources/icons/ok.svg")
            :icons/public              (slurp-svg "./resources/icons/public.svg")
            :icons/public-chat         (slurp-svg "./resources/icons/public_chat.svg")
            :icons/qr                  (slurp-svg "./resources/icons/QR.svg")
            :icons/search              (slurp-svg "./resources/icons/search.svg")
            :icons/smile               (slurp-svg "./resources/icons/smile.svg")
            :icons/commands-list       (slurp-svg "./resources/icons/commands_list.svg")
            :icons/dropdown-up         (slurp-svg "./resources/icons/dropdown_up.svg")
            :icons/dropdown            (slurp-svg "./resources/icons/dropdown.svg")
            :icons/grab                (slurp-svg "./resources/icons/grab.svg")})

(defn normalize-property-name [n]
  (if (= n :icons/options)
    (if ios? :icons/dots-horizontal :icons/dots-vertical)
    n))

(def default-viewbox {:width 24 :height 24 :viewBox "0 0 24 24"})

(defn icon
  ([name] (icon name nil))
  ([name {:keys [color container-style style]}]
   [rn/view container-style
    (if-let [icon-fn (get icons (normalize-property-name name))]
      (into []
            (concat
              [svg (merge default-viewbox style)]
              (icon-fn
                (cond
                  (keyword? color)
                  (case color
                    :dark st/icon-dark-color
                    :gray st/icon-gray-color
                    :blue st/color-light-blue
                    :active st/color-blue4
                    :white st/color-white
                    :red st/icon-red-color
                    st/icon-dark-color)
                  (string? color)
                  color
                  :else
                  st/icon-dark-color))))
      (throw (js/Error. (str "Unknown icon: " name))))]))

(defn touchable-icon [n opts handler]
  [rn/touchable-highlight {:on-press handler}
   [rn/view
    [icon n opts]]])