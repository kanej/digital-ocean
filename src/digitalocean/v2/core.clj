(ns digitalocean.v2.core
  (:import [java.net URLEncoder])
  (:refer-clojure :exclude [keys])
  (:require [cheshire.core :as json]
	    [schema.core :as scm]
            [clojure.java.io :as io]
	    [org.httpkit.client :as http]))

;; _____ _____ _____ _____ _______       _         ____   _____ ______          _   _
;; |  __ \_   _/ ____|_   _|__   __|/\   | |       / __ \ / ____|  ____|   /\   | \ | |
;; | |  | || || |  __  | |    | |  /  \  | |      | |  | | |    | |__     /  \  |  \| |
;; | |  | || || | |_ | | |    | | / /\ \ | |      | |  | | |    |  __|   / /\ \ | . ` |
;; | |__| || || |__| |_| |_   | |/ ____ \| |____  | |__| | |____| |____ / ____ \| |\  |
;; |_____/_____\_____|_____|  |_/_/    \_\______|  \____/ \_____|______/_/    \_\_| \_|


(def endpoint "https://api.digitalocean.com/v2/")

(defn load-dev-token
  "Loads temporary token for development
   can be removed at some point"
  []
  (let [path "/Users/owainlewis/.auth/DIGITALOCEAN.txt"]
    (with-open [rdr (io/reader path)]
      (first (take 1 (line-seq rdr))))))

;; **************************************************************

(defn run-request
  "Utility method for making HTTP requests
   to the Digital Ocean API"
  [method url token & params]
  (let [all-params (into {} params)
        headers {"Content-Type" "application/json"
                 "Authorization" (str "Bearer " token)}
        mergeable (if (empty? all-params)
                    (hash-map)
                    {:body (json/encode all-params)})
        {:keys [status headers body error] :as resp}
          @(http/request
            (merge
              {:method method
               :url url
               :headers headers} mergeable))]
  (if (nil? error)
    (json/parse-string body true)
    {:error error})))

(defn normalize-url [url]
  (if (string? url)
    (URLEncoder/encode
      (clojure.string/lower-case url))
    url))

(defn resource-url
  "Helper function that builds url endpoints
   (resource-url :domains 1 2 3) =>
     https://api.digitalocean.com/v2/domains/1/2/3
  "
  [resource & parts]
  (let [nested-url-parts
         (apply str
           (interpose "/"
             (map normalize-url (into [] parts))))
        qualified-resource (name resource)]
    (str endpoint qualified-resource "/" nested-url-parts)))

;; Generics
;; **************************************************************

(defn generic
  "The function does the bulk of the work in abstracting away repetitive
   REST like requests.
   i.e (generic :get :domains) => (fn [token] ;; domain fetching logic)"
  [method resource]
  (let [request-builder (fn [token url-identifiers & params]
                          (let [resource-endpoint
                            (-> (partial resource-url (name resource))
                                (apply url-identifiers))]
    (run-request method
                 resource-endpoint
                 token
                 (into {} params))))]
  (fn
    ([token]
      (request-builder token [] {}))
    ([token resource-identifier & params]
      (request-builder token [resource-identifier] (into {} params))))))

;; Actions
;; **************************************************************

(def actions
  "List all actions that have been executed on the current account."
  (generic :get :actions))

(def get-action
  "Get a single action"
  actions)

;; Domain Records
;; **************************************************************

(defn records
  "Return all records for a domain"
  [token domain]
  (run-request :get
    (resource-url (str "domains/" domain "/records"))
      token))

;; Domains
;; **************************************************************

(def domains
  "Fetch all domains"
  (generic :get :domains))

(def get-domain
  "Get a single domain by name"
  domains)

;; Droplet Actions
;; **************************************************************

(defn- droplet-action
  "Constructor function that builds RESTful droplet action functions."
  [action]
  (fn [token droplet-id]
    (run-request :post
      (resource-url (str "droplets/" droplet-id "/actions"))
      token
      {:type action})))

(def reboot-droplet
  "Reboot a droplet"
  (droplet-action "reboot"))

(def power-cycle-droplet
  "Power cycle a droplet (power off and then back on)"
  (droplet-action "power_cycle"))

(def shutdown-droplet
  "Shutdown a droplet. A shutdown action is an attempt to shutdown
  the Droplet in a graceful way."
  (droplet-action "shutdown"))

(def power-off-droplet
  "Powers off a droplet. A power off is a hard shutdown and should
   only be used if the shutdown action is not successful. It is
   similar to cutting the power on a server and could lead to
   complications."
  (droplet-action "power_off"))

(def power-on-droplet
  "Powers on a droplet."
  (droplet-action "power_on"))

(def password-reset-droplet
  "Reset the password for a droplet"
  (droplet-action "password_reset"))

;; Droplets
;; **************************************************************

(def droplets
  "Get all droplets"
  (generic :get (str "droplet/" )))

(defn get-droplet-kernels
  "Retrieve a list of all kernels available to a Dropet"
  [token droplet-id]
  (run-request :post
    (resource-url (str "droplets/" droplet-id "/kernels")) token))

(defn get-droplet-snapshots
  "retrieve the snapshots that have been created from a Droplet"
  [token droplet-id]
  (run-request :post
    (resource-url (str "droplets/" droplet-id "/snapshots")) token))

(defn get-droplet-backups
  "Retrieve any backups associated with a Droplet"
  [token droplet-id]
  (run-request :post
    (resource-url (str "droplets/" droplet-id "/backups")) token))

(defn get-droplet-actions
  "Retrieve all actions that have been executed on a Droplet"
  [token droplet-id]
  (run-request :post
    (resource-url (str "droplets/" droplet-id "/backups")) token))

(def create-droplet
  "Create a new droplet"
  (generic :post :droplets))

(def get-droplet
  "Get a single droplet by ID"
  droplets)

(def destroy-droplet
  "Delete a Droplet"
  (generic :delete :droplets))

;; Images
;; **************************************************************

(def images "Return all images"
  (generic :get :images))

(def get-image images)

;; Keys
;; **************************************************************

(def ssh-keys "Get all account SSH keys"
  (generic :get "account/keys"))

(def create-key
  "Create a new SSH key"
  (generic :post "account/keys"))

(def get-key ssh-keys)

(def update-key
  "Update the name of an SSH key"
  (generic :put "account/keys"))

(def destroy-key
  "Destroy a public SSH key that you have in your account"
  (generic :delete "account/keys"))

;; Regions
;; **************************************************************

(def regions
  "Returns all Digital Ocean regions"
  (generic :get :regions))

;; Sizes
;; **************************************************************

(def sizes
  "Returns droplet sizes for Digital Ocean images"
  (generic :get :sizes))
