DROP DATABASE rpserver;
CREATE DATABASE rpserver;
\c rpserver
CREATE TABLE IF NOT EXISTS rider (
    id                 BIGSERIAL        PRIMARY KEY,
    uuid               VARCHAR(200) ,
    nickname           VARCHAR(50)  UNIQUE ,
    password	       VARCHAR(200)   NOT NULL,
    refresh_token	   VARCHAR(200)   ,
    exp_time           TIMESTAMP,
    bio                TEXT          ,
    profile_image_url  VARCHAR(200)   ,
    hometown           VARCHAR(100)                  ,
    registration_date  TIMESTAMP WITH TIME ZONE      DEFAULT NOW(),
    ocassion_id        INT                           UNIQUE
);

CREATE TABLE IF NOT EXISTS spot (
    id                  BIGSERIAL       PRIMARY KEY,
    title               VARCHAR(50)  NOT NULL    UNIQUE,
    coordinates         POINT        NOT NULL    ,
    notes               TEXT,
    profile_image_url   VARCHAR(200)
);

CREATE TABLE IF NOT EXISTS ocassion (
    id     SERIAL       PRIMARY KEY,
    title           VARCHAR(50)  NOT NULL	UNIQUE,
    description     TEXT         NOT NULL,
    when_date       TIMESTAMP WITH TIME ZONE    NOT NULL,
    rider_id        BIGINT      NOT NULL,
    spot_id         BIGINT      NOT NULL,
    FOREIGN KEY (rider_id) REFERENCES rider(id),
    FOREIGN KEY (spot_id) REFERENCES spot(id)
);
