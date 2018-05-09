(ns status-im.ui.screens.offline-messaging-settings.events
  (:require [re-frame.core :refer [dispatch subscribe]]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.handlers-macro :as handlers-macro]
            [status-im.ui.screens.accounts.events :as accounts-events]
            [status-im.i18n :as i18n]
            [status-im.transport.core :as transport]
            [status-im.utils.ethereum.core :as ethereum]
            [status-im.ui.screens.wallet.send.events :as send-events]
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
 :save-transaction
 (fn [{:keys [db] :as cofx} [_ wnode hash]]
   (let [network     (ethereum/network->chain-keyword (:network db))
         settings    (get-in db [:account/account :settings])
         transaction @(subscribe [:wallet.transactions/transaction-details])]
     (when (= hash (:hash transaction))
       (handlers-macro/merge-fx cofx
                                {:dispatch [::save-wnode wnode hash]}
                                (accounts-events/update-settings (assoc-in settings [:wnode-payment network wnode] hash)))))))

(handlers/register-handler-fx
 :sent-wnode-tx
 (fn [{:keys [db] :as cofx} [_ wnode hash]]
   (let [network  (ethereum/network->chain-keyword (:network db))
         settings (get-in db [:account/account :settings])]
     (handlers-macro/merge-fx cofx
                              {:db       (assoc-in db [:wallet :current-transaction] hash)
                               :dispatch [:save-transaction wnode hash]}))))

(handlers/register-handler-fx
 ::pay-wnode
 (fn [{{:keys [web3] :as db} :db} [_ wnode]]
   (let [db'                             (assoc-in db [:wallet :send-transaction :wrong-password?] false)
         network                         (ethereum/network->chain-keyword (:network db))
         {:keys [address amount symbol]} (get-in db [:inbox/wnodes network wnode :payment])]
     {:db                            (update-in db' [:wallet :send-transaction] assoc :waiting-signal? false)
      ::send-events/send-transaction {:web3      web3
                                      :from      (get-in db [:account/account :address])
                                      :to        address
                                      :value     amount
                                      :gas       (ethereum/estimate-gas symbol)
                                      :gas-price (money/->wei :gwei 5)
                                      :symbol    symbol
                                      :network   network
                                      :on-sent   #(dispatch [:sent-wnode-tx wnode %2])}})))

(handlers/register-handler-fx
 :select-wnode
 (fn [{:keys [db]} [_ wnode]]
   (let [network    (ethereum/network->chain-keyword (:network db))
         payment    (get-in db [:inbox/wnodes network wnode :payment])
         payment-tx (get-in db [:account/account :settings :wnode-payment network wnode])]
     (if (nil? payment)
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
