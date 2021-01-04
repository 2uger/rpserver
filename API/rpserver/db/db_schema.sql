CREATE TABLE IF NOT EXISTS rider (
    rider_id           SERIAL        PRIMARY KEY,
    nickname           VARCHAR(50)   NOT NULL,
    surname            VARCHAR(50),
    email              VARCHAR(100)  NOT NULL   UNIQUE,
    password           VARCHAR(100)         NOT NULL,
    bio                TEXT          ,
    profile_image_url  VARCHAR(50)   ,
    hometown           VARCHAR(100)  NOT NULL,
    registration_date  DATE          NOT NULL,
    ocassion_id        INT                      UNIQUE
);

CREATE TABLE IF NOT EXISTS  spot (
    spot_id             SERIAL       PRIMARY KEY,
    nickname            VARCHAR(50)  NOT NULL    UNIQUE,
    coordinates         POINT        NOT NULL    ,
    notes               TEXT,
    profile_image_url   VARCHAR(50)
);

CREATE TABLE IF NOT EXISTS ocassion (
    ocassion_id     SERIAL       PRIMARY KEY,
    nickname        VARCHAR(50)  NOT NULL	UNIQUE,
    description     TEXT         NOT NULL,
    when_date       TIMESTAMP    NOT NULL,
    rider_id        INTEGER      NOT NULL,
    spot_id         INTEGER      NOT NULL
);

CREATE TABLE IF NOT EXISTS rider_relation (
    rider_relation_id   SERIAL      PRIMARY KEY,
    rider_id            INTEGER     NOT NULL,
    relative_rider_id   INTEGER     NOT NULL,
    realation_type      INTEGER     NOT NULL
);

CREATE TABLE IF NOT EXISTS blacklist_token (
    token_id        SERIAL        PRIMARY KEY,
    token_value     VARCHAR(200)  NOT NULL,
    rider_id        INTEGER       NOT NULL,
    expiration_time TIMESTAMP     NOT NULL
);

ALTER TABLE rider
ADD CONSTRAINT rider_ocassion_id FOREIGN KEY (ocassion_id) REFERENCES ocassion(ocassion_id);

ALTER TABLE rider_relation
ADD CONSTRAINT relation_rider_id FOREIGN KEY (rider_id) REFERENCES rider(rider_id),
ADD CONSTRAINT relation_relative_rider_id FOREIGN KEY (relative_rider_id) REFERENCES rider(rider_id);

ALTER TABLE ocassion 
ADD CONSTRAINT ocassion_rider_id FOREIGN KEY (rider_id) REFERENCES rider(rider_id),
ADD CONSTRAINT ocassion_spot_id FOREIGN KEY (spot_id) REFERENCES spot(spot_id);

ALTER TABLE blacklist_token
ADD CONSTRAINT token_rider_id FOREIGN KEY (rider_id) REFERENCES rider(rider_id);


