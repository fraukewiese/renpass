-- MySQL dump 10.13  Distrib 5.5.37, for debian-linux-gnu (i686)
--
-- Host: localhost    Database: results
-- ------------------------------------------------------
-- Server version	5.5.37-0ubuntu0.12.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `results`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `results` /*!40100 DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci */;

USE `results`;

--
-- Table structure for table `co2`
--

DROP TABLE IF EXISTS `co2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `co2` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `co2` float(15,2) NOT NULL COMMENT 't',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `demand`
--

DROP TABLE IF EXISTS `demand`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `demand` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `demand` float(15,2) NOT NULL COMMENT 'MW',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `electricity_production`
--

DROP TABLE IF EXISTS `electricity_production`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `electricity_production` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `type` varchar(15) COLLATE utf8_unicode_ci NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `electricity_production` float(15,2) NOT NULL COMMENT '[MW] possible production including excess_vre',
  PRIMARY KEY (`scenario_nr`,`timestep`,`type`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `excess_vre_after_exchange`
--

DROP TABLE IF EXISTS `excess_vre_after_exchange`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `excess_vre_after_exchange` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `excess_vre` int(13) NOT NULL COMMENT '[MW]',
  PRIMARY KEY (`scenario_nr`,`timestep`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `excess_vre_after_storage`
--

DROP TABLE IF EXISTS `excess_vre_after_storage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `excess_vre_after_storage` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `excess_vre` float NOT NULL COMMENT '[MW]',
  PRIMARY KEY (`scenario_nr`,`timestep`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `exchange`
--

DROP TABLE IF EXISTS `exchange`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `exchange` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `plus_dpr_number` int(10) unsigned NOT NULL COMMENT 'flow from that region is positive',
  `minus_dpr_number` int(10) unsigned NOT NULL COMMENT 'flow from that region is negative',
  `capacity_used` int(13) NOT NULL COMMENT '[MW] if positive: from plus to minus region. if negative: from minus to plus region',
  `grid_loss_rel` decimal(3,2) NOT NULL COMMENT '[ ] relative to the capacity used, additional to it',
  `grid_loss_abs` float(7,2) NOT NULL COMMENT '[MW] additional to the capacity used, only absolute positive values',
  PRIMARY KEY (`scenario_nr`,`timestep`,`plus_dpr_number`,`minus_dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `exchange_after_storage`
--

DROP TABLE IF EXISTS `exchange_after_storage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `exchange_after_storage` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `plus_dpr_number` int(10) unsigned NOT NULL COMMENT 'flow from that region is positive',
  `minus_dpr_number` int(10) unsigned NOT NULL COMMENT 'flow from that region is negative',
  `capacity_used` int(13) NOT NULL COMMENT '[MW] if positive: from plus to minus region. if negative: from minus to plus region',
  `grid_loss_rel` decimal(3,2) NOT NULL COMMENT '[ ] relative to the capacity used, additional to it',
  `grid_loss_abs` float(7,2) NOT NULL COMMENT '[MW] additional to the capacity used, only absolute positive values',
  PRIMARY KEY (`scenario_nr`,`timestep`,`plus_dpr_number`,`minus_dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `filling_level`
--

DROP TABLE IF EXISTS `filling_level`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `filling_level` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `type` varchar(20) COLLATE utf8_unicode_ci NOT NULL COMMENT 'of storage like ''hydro'',''gas'',''caes''',
  `filling_level` float(15,2) NOT NULL COMMENT 'hydro: mio cbm',
  `filling_level_energy` float NOT NULL COMMENT 'GWh',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`,`type`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `filling_level_indicator`
--

DROP TABLE IF EXISTS `filling_level_indicator`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `filling_level_indicator` (
  `scenario_nr` mediumint(5) unsigned NOT NULL,
  `res_nr` mediumint(5) unsigned NOT NULL,
  `max` float NOT NULL COMMENT 'max relative filling level in calculations',
  `min` float NOT NULL COMMENT 'min relative filling level in calculations',
  `mean` float NOT NULL COMMENT 'mean relative filling level in calculations',
  `end` float NOT NULL COMMENT 'end relative filling level',
  `max_delta` float NOT NULL COMMENT 'absolute max filling level change',
  PRIMARY KEY (`scenario_nr`,`res_nr`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `over_demand`
--

DROP TABLE IF EXISTS `over_demand`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `over_demand` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `over_demand` float(15,2) NOT NULL COMMENT 'MW',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `price_after_exchange`
--

DROP TABLE IF EXISTS `price_after_exchange`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `price_after_exchange` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `price` float(15,2) NOT NULL COMMENT '[EURO/MWh] price after the exchange',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `price_before_exchange`
--

DROP TABLE IF EXISTS `price_before_exchange`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `price_before_exchange` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `price` float(15,2) NOT NULL COMMENT '[EURO/MWh] price before the exchange',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `residual_load_before_exchange`
--

DROP TABLE IF EXISTS `residual_load_before_exchange`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `residual_load_before_exchange` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `residual_load` float(15,2) NOT NULL COMMENT '[MW] before the exchange, negative values are excess_vre before the exchange',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `storage_consumption`
--

DROP TABLE IF EXISTS `storage_consumption`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `storage_consumption` (
  `scenario_nr` int(5) unsigned NOT NULL,
  `timestep` mediumint(5) unsigned NOT NULL,
  `dpr_number` int(10) unsigned NOT NULL,
  `type` varchar(20) COLLATE utf8_unicode_ci NOT NULL COMMENT 'of storage like ''hydro'',''gas'',''caes''',
  `storage_consumption` float(15,2) NOT NULL COMMENT 'MW',
  PRIMARY KEY (`scenario_nr`,`timestep`,`dpr_number`,`type`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2014-04-29 18:22:20
