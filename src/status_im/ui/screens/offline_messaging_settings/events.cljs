(ns status-im.ui.screens.offline-messaging-settings.events
  (:require [re-frame.core :refer [dispatch]]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.handlers-macro :as handlers-macro]
            [status-im.ui.screens.accounts.events :as accounts-events]
            [status-im.i18n :as i18n]
            [status-im.utils.ethereum.core :as ethereum]
            [status-im.utils.ethereum.erc20 :as erc20]
            [status-im.utils.ethereum.tokens :as tokens]
            [status-im.utils.money :as money]))

(handlers/register-handler-fx
 ::save-wnode
 (fn [{:keys [db now] :as cofx} [_ wnode]]
   (let [network  (ethereum/network->chain-keyword (:network db))
         settings (get-in db [:account/account :settings])]
     (handlers-macro/merge-fx cofx
                              {:dispatch [:logout]}
                              (accounts-events/update-settings (assoc-in settings [:wnode network] wnode))))))

(handlers/register-handler-fx
 :save-wnode-transaction
 (fn [{:keys [db] :as cofx} [_ wnode hash]]
   (let [network     (ethereum/network->chain-keyword (:network db))
         settings    (get-in db [:account/account :settings])
         transaction (get-in db [:wallet :transactions hash])]
     (when (not (nil? transaction))
       (handlers-macro/merge-fx cofx
                                {:dispatch [::save-wnode wnode hash]}
                                (accounts-events/update-settings (assoc-in settings [:wnode-payment network wnode] hash)))))))

(handlers/register-handler-fx
 ::pay-wnode
 (fn [{{:keys [web3] :as db} :db} [_ wnode]]
   (let [network                         (ethereum/network->chain-keyword (:network db))
         {:keys [address amount symbol]} (get-in db [:inbox/wnodes network wnode :payment])
         contract                        (:address (tokens/symbol->token network symbol))
         from                            (get-in db [:account/account :address])
         gas                             (ethereum/estimate-gas symbol)
         gas-price                       (money/->wei :gwei 5)]
     {:db (update-in db [:wallet :send-transaction] assoc :waiting-signal? false)}
     (erc20/transfer web3
                     contract
                     from
                     address
                     amount
                     {:gas gas :gasPrice gas-price}
                     #(dispatch [:save-wnode-transaction wnode %2])))))

(handlers/register-handler-fx
 :select-wnode
 (fn [{:keys [db]} [_ wnode]]
   (let [network    (ethereum/network->chain-keyword (:network db))
         payment    (get-in db [:inbox/wnodes network wnode :payment])
         payment-tx (get-in db [:account/account :settings :wnode-payment network wnode])]
     (if (or (nil? payment) (not (nil? payment-tx)))
       {:dispatch [::save-wnode wnode]}
       {:dispatch [::pay-wnode wnode]}))))

(handlers/register-handler-fx
 :connect-wnode
 (fn [{:keys [db]} [_ wnode]]
   (let [network (ethereum/network->chain-keyword (:network db))]
     {:show-confirmation {:title               (i18n/label :t/close-app-title)
                          :content             (i18n/label :t/connect-wnode-content
                                                           {:name (get-in db [:inbox/wnodes network wnode :name])})
                          :confirm-button-text (i18n/label :t/close-app-button)
                          :on-accept           #(dispatch [:select-wnode wnode])
                          :on-cancel           nil}})))
