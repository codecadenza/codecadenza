/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.testing.util;

import java.io.File;
import java.util.ArrayList;
import java.util.UUID;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.ClientPlatform;
import net.codecadenza.eclipse.testing.domain.ContentType;
import net.codecadenza.eclipse.testing.domain.DataExchangeMethod;
import net.codecadenza.eclipse.testing.domain.DataSource;
import net.codecadenza.eclipse.testing.domain.DatabaseVendor;
import net.codecadenza.eclipse.testing.domain.DomainAssociation;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.EnumAssociation;
import net.codecadenza.eclipse.testing.domain.Enumeration;
import net.codecadenza.eclipse.testing.domain.Project;
import net.codecadenza.eclipse.testing.domain.ProjectType;
import net.codecadenza.eclipse.testing.domain.TechnologyPlatform;

/**
 * <p>
 * Factory that creates the project meta-model for the test
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectFactory {
	private static final String ENUM_APP_STATUS = "ApplicationStatus";
	private static final String DEFAULT_PROJECT_NAME = "app-store";
	private static final int DEFAULT_START_Y_POS = 20;
	private static final String DB_DRIVER_NAME_MYSQL = "com.mysql.cj.jdbc.Driver";
	private static final String DB_DRIVER_NAME_POSTGRESQL = "org.postgresql.Driver";
	private static final String DB_PASSWORD = "sa";
	private static final String DB_NAME_PREFIX = "db_";
	private static final String DB_SCHEMA_PREFIX = "ui_test_";
	private static final String DB_URL_MYSQL = "jdbc:mysql://localhost:3306";
	private static final String DB_URL_POSTGRESQL = "jdbc:postgresql://localhost:5432/";
	private static final String DB_USER_NAME = "sa";
	private static final String DOMAIN_OBJECT_ADDRESS = "Address";
	private static final String DOMAIN_OBJECT_APPLICATION = "Application";
	private static final String DOMAIN_OBJECT_CONF_ITEM = "ConfigurationItem";
	private static final String DOMAIN_OBJECT_CONF_GROUP = "ConfigurationGroup";
	private static final String DOMAIN_OBJECT_CUSTOMER = "Customer";
	private static final String DOMAIN_OBJECT_BASE_ENTITY = "BaseEntity";
	private static final String DOMAIN_OBJECT_DEPARTMENT = "Department";
	private static final String DOMAIN_OBJECT_OFFICE = "Office";
	private static final String DOMAIN_OBJECT_ROLE = "Role";
	private static final String DOMAIN_OBJECT_SUPPLIER = "Supplier";
	private static final String DOMAIN_OBJECT_USER = "User";
	private static final String LOCAL_JDBC_LIB_PATH = "/development/lib/jdbc/";

	/**
	 * Prevent instantiation
	 */
	private ProjectFactory() {
	}

	/**
	 * Create a project meta-model for the test
	 * @param name
	 * @param projectType
	 * @param technologyPlatform
	 * @param clientPlatform
	 * @param databaseVendor
	 * @return the new project
	 */
	public static Project createProject(String name, ProjectType projectType, TechnologyPlatform technologyPlatform,
			ClientPlatform clientPlatform, DatabaseVendor databaseVendor) {
		final var project = new Project(name, technologyPlatform, clientPlatform);

		if (projectType == ProjectType.MANY_TO_ONE) {
			final var confGroupDomainObject = createConfGroupDomainObject();
			final var confItemDomainObject = createConfItemDomainObject(confGroupDomainObject);
			final var confGroupImportMethod = new DataExchangeMethod(confGroupDomainObject, ContentType.CSV, false, true);

			project.getDomainObjects().add(confGroupDomainObject);
			project.getDomainObjects().add(confItemDomainObject);
			project.getDataExchangeMethods().add(confGroupImportMethod);
		}
		else if (projectType == ProjectType.BIDIRECTIONAL_ONE_TO_MANY) {
			final var confItemDomainObject = createConfItemDomainObject(null);
			final var applicationDomainObject = createApplicationDomainObject(confItemDomainObject, null, null);
			final var applicationImportMethod = new DataExchangeMethod(applicationDomainObject, ContentType.XML, false, true);

			project.getDomainObjects().add(applicationDomainObject);
			project.getDomainObjects().add(confItemDomainObject);
			project.getDataExchangeMethods().add(applicationImportMethod);
		}
		else if (projectType == ProjectType.UNIDIRECTIONAL_ONE_TO_MANY) {
			final var officeDomainObject = createOfficeDomainObject(null);
			final var departmentDomainObject = createDepartmentDomainObject(officeDomainObject, null);
			final var departmentImportMethod = new DataExchangeMethod(departmentDomainObject, ContentType.JSON, true, true);

			project.getDomainObjects().add(officeDomainObject);
			project.getDomainObjects().add(departmentDomainObject);
			project.getDataExchangeMethods().add(departmentImportMethod);
		}
		else if (projectType == ProjectType.ENUM) {
			final var statusEnum = createStatusEnumeration(null);
			final var applicationDomainObject = createApplicationDomainObject(null, statusEnum, null);
			final var applicationImportMethod = new DataExchangeMethod(applicationDomainObject, ContentType.XML, false, true);

			project.getDomainObjects().add(applicationDomainObject);
			project.getEnumerations().add(statusEnum);
			project.getDataExchangeMethods().add(applicationImportMethod);
		}
		else if (projectType == ProjectType.ONE_TO_ONE) {
			final var addressDomainObject = createAddressDomainObject(null);
			final var customerDomainObject = createCustomerDomainObject(addressDomainObject, null);
			final var customerExportMethod = new DataExchangeMethod(customerDomainObject, ContentType.EXCEL2007, false, false);

			project.getDomainObjects().add(addressDomainObject);
			project.getDomainObjects().add(customerDomainObject);
			project.getDataExchangeMethods().add(customerExportMethod);
		}
		else if (projectType == ProjectType.MANY_TO_MANY) {
			final var roleDomainObject = createRoleDomainObject(null);
			final var userDomainObject = createUserDomainObject(roleDomainObject, null);
			final var userExportMethod = new DataExchangeMethod(userDomainObject, ContentType.JSON, true, false);

			project.getDomainObjects().add(roleDomainObject);
			project.getDomainObjects().add(userDomainObject);
			project.getDataExchangeMethods().add(userExportMethod);
		}
		else if (projectType == ProjectType.INHERITANCE) {
			final var mappedSuperClass = createMappedSuperClass();
			final var supplierDomainObject = createSupplierDomainObject(mappedSuperClass);

			project.getDomainObjects().add(mappedSuperClass);
			project.getDomainObjects().add(supplierDomainObject);
		}
		else
			throw new IllegalArgumentException("The project type '" + projectType + "' is not supported!");

		project.setDataSource(initDataSource(databaseVendor));

		return project;
	}

	/**
	 * Create a project meta-model for the test
	 * @param name
	 * @param technologyPlatform
	 * @param clientPlatform
	 * @param databaseVendor
	 * @return the new project
	 */
	public static Project createProject(String name, TechnologyPlatform technologyPlatform, ClientPlatform clientPlatform,
			DatabaseVendor databaseVendor) {
		final Project project = new Project(name, technologyPlatform, clientPlatform);
		final var confGroupDomainObject = createConfGroupDomainObject();
		final var confItemDomainObject = createConfItemDomainObject(confGroupDomainObject);
		final var statusEnum = createStatusEnumeration(150);
		final var applicationDomainObject = createApplicationDomainObject(confItemDomainObject, statusEnum, 150);
		final var addressDomainObject = createAddressDomainObject(300);
		final var customerDomainObject = createCustomerDomainObject(addressDomainObject, 300);
		final var roleDomainObject = createRoleDomainObject(450);
		final var userDomainObject = createUserDomainObject(roleDomainObject, 450);

		final var confGroupImportMethod = new DataExchangeMethod(confGroupDomainObject, ContentType.CSV, false, true);
		final var applicationImportMethod = new DataExchangeMethod(applicationDomainObject, ContentType.XML, false, true);
		final var customerExportMethod = new DataExchangeMethod(customerDomainObject, ContentType.EXCEL2007, false, false);
		final var userExportMethod = new DataExchangeMethod(userDomainObject, ContentType.JSON, true, false);

		project.getDomainObjects().add(confGroupDomainObject);
		project.getDomainObjects().add(confItemDomainObject);
		project.getDomainObjects().add(applicationDomainObject);
		project.getEnumerations().add(statusEnum);
		project.getDomainObjects().add(addressDomainObject);
		project.getDomainObjects().add(customerDomainObject);
		project.getDomainObjects().add(roleDomainObject);
		project.getDomainObjects().add(userDomainObject);
		project.getDataExchangeMethods().add(applicationImportMethod);
		project.getDataExchangeMethods().add(confGroupImportMethod);
		project.getDataExchangeMethods().add(customerExportMethod);
		project.getDataExchangeMethods().add(userExportMethod);
		project.setDataSource(initDataSource(databaseVendor));

		return project;
	}

	/**
	 * Create a simple project that contains neither integration beans nor Selenium tests
	 * @return the new project
	 */
	public static Project createProject() {
		return createProject(DEFAULT_PROJECT_NAME, ProjectType.ENUM, TechnologyPlatform.JAVA_SE, ClientPlatform.JAVAFX,
				DatabaseVendor.MYSQL);
	}

	/**
	 * Create the configuration item domain object
	 * @param confGroupDomainObject
	 * @return the new domain object
	 */
	private static DomainObject createConfItemDomainObject(DomainObject confGroupDomainObject) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, false);
		final var attrValue = new DomainAttribute("value", DomainAttribute.TYPE_STRING, false, false);

		final var confItemDomainObject = new DomainObject(DOMAIN_OBJECT_CONF_ITEM);
		confItemDomainObject.setLabel("configuration item");
		confItemDomainObject.setPluralLabel("Configuration items");
		confItemDomainObject.setTableName("configuration_item_tab");
		confItemDomainObject.addAttribute(attrId);
		confItemDomainObject.addAttribute(attrName);
		confItemDomainObject.addAttribute(attrValue);
		confItemDomainObject.setStartXPosition(500);
		confItemDomainObject.setStartYPosition(20);

		if (confGroupDomainObject != null) {
			final var mtoGroup = new DomainAssociation("configurationGroup", confGroupDomainObject, AssociationType.MANY_TO_ONE, false);
			confItemDomainObject.addAssociation(mtoGroup);
		}

		return confItemDomainObject;
	}

	/**
	 * Create the configuration group domain object
	 * @return the new domain object
	 */
	private static DomainObject createConfGroupDomainObject() {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);

		final var confGroupDomainObject = new DomainObject(DOMAIN_OBJECT_CONF_GROUP);
		confGroupDomainObject.setLabel("configuration group");
		confGroupDomainObject.setPluralLabel("Configuration groups");
		confGroupDomainObject.setTableName("configuration_group_tab");
		confGroupDomainObject.addAttribute(attrName);
		confGroupDomainObject.addAttribute(attrId);
		confGroupDomainObject.setStartXPosition(20);
		confGroupDomainObject.setStartYPosition(20);
		confGroupDomainObject.setCreateDefaultForms(true);

		return confGroupDomainObject;
	}

	/**
	 * Create the user domain object
	 * @param roleDomainObject
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createUserDomainObject(DomainObject roleDomainObject, Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);
		final var attrActive = new DomainAttribute("active", DomainAttribute.TYPE_BOOLEAN, false, false);
		final var attrCreationDate = new DomainAttribute("creation", DomainAttribute.TYPE_DATE, false, false);
		final var attrLastUpdate = new DomainAttribute("updated", DomainAttribute.TYPE_CALENDAR, false, false);
		final var attrActivationDate = new DomainAttribute("activation", DomainAttribute.TYPE_LOCAL_DATE, false, false);
		final var attrLockDate = new DomainAttribute("locking", DomainAttribute.TYPE_LOCAL_DATE_TIME, false, false);
		final var attrExternalId = new DomainAttribute("external", DomainAttribute.TYPE_UUID, false, false);
		final var attrLicenseCosts = new DomainAttribute("license", DomainAttribute.TYPE_BIG_DECIMAL, false, false);
		final var mtmRoles = new DomainAssociation("roles", roleDomainObject, AssociationType.MANY_TO_MANY, false);

		final var userDomainObject = new DomainObject(DOMAIN_OBJECT_USER);
		userDomainObject.addAttribute(attrName);
		userDomainObject.addAttribute(attrActive);
		userDomainObject.addAttribute(attrCreationDate);
		userDomainObject.addAttribute(attrLastUpdate);
		userDomainObject.addAttribute(attrActivationDate);
		userDomainObject.addAttribute(attrLockDate);
		userDomainObject.addAttribute(attrExternalId);
		userDomainObject.addAttribute(attrLicenseCosts);
		userDomainObject.addAttribute(attrId);
		userDomainObject.addAssociation(mtmRoles);
		userDomainObject.setStartXPosition(20);
		userDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		return userDomainObject;
	}

	/**
	 * Create the application domain object
	 * @param confItemDomainObject
	 * @param statusEnum
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createApplicationDomainObject(DomainObject confItemDomainObject, Enumeration statusEnum,
			Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);

		final var appDomainObject = new DomainObject(DOMAIN_OBJECT_APPLICATION);
		appDomainObject.addAttribute(attrName);
		appDomainObject.addAttribute(attrId);
		appDomainObject.setStartXPosition(20);
		appDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		if (confItemDomainObject != null) {
			final var otmConfItems = new DomainAssociation("configurationItems", confItemDomainObject, AssociationType.ONE_TO_MANY,
					true);
			appDomainObject.addAssociation(otmConfItems);
		}

		if (statusEnum != null)
			appDomainObject.addEnumAssociation(new EnumAssociation("status", statusEnum));

		return appDomainObject;
	}

	/**
	 * Create the role domain object
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createRoleDomainObject(Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);

		final var roleDomainObject = new DomainObject(DOMAIN_OBJECT_ROLE);
		roleDomainObject.addAttribute(attrName);
		roleDomainObject.addAttribute(attrId);
		roleDomainObject.setStartXPosition(500);
		roleDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);
		roleDomainObject.setCreateDefaultForms(true);

		return roleDomainObject;
	}

	/**
	 * Create the status enumeration
	 * @param startYPosition
	 * @return the new enumeration
	 */
	private static Enumeration createStatusEnumeration(Integer startYPosition) {
		final var statusEnum = new Enumeration(ENUM_APP_STATUS);
		statusEnum.addLiteral("NEW");
		statusEnum.addLiteral("ACTIVE");
		statusEnum.addLiteral("DO_NOT_USE");
		statusEnum.setStartXPosition(500);
		statusEnum.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		return statusEnum;
	}

	/**
	 * Create the address domain object
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createAddressDomainObject(Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrZip = new DomainAttribute("zip", DomainAttribute.TYPE_STRING, false, false);
		final var attrCity = new DomainAttribute("city", DomainAttribute.TYPE_STRING, false, false);
		final var attrStreet = new DomainAttribute("street", DomainAttribute.TYPE_STRING, false, false);

		final var addressDomainObject = new DomainObject(DOMAIN_OBJECT_ADDRESS);
		addressDomainObject.setPluralLabel(DOMAIN_OBJECT_ADDRESS + "es");
		addressDomainObject.addAttribute(attrZip);
		addressDomainObject.addAttribute(attrCity);
		addressDomainObject.addAttribute(attrStreet);
		addressDomainObject.addAttribute(attrId);
		addressDomainObject.setStartXPosition(500);
		addressDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		return addressDomainObject;
	}

	/**
	 * Create the customer domain object
	 * @param domainObjectAddress
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createCustomerDomainObject(DomainObject domainObjectAddress, Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);
		final var attrCredit = new DomainAttribute("credit", DomainAttribute.TYPE_DOUBLE, false, false);
		final var otoAddress = new DomainAssociation("address", domainObjectAddress, AssociationType.ONE_TO_ONE, true);

		final var customerDomainObject = new DomainObject(DOMAIN_OBJECT_CUSTOMER);
		customerDomainObject.addAttribute(attrName);
		customerDomainObject.addAttribute(attrCredit);
		customerDomainObject.addAttribute(attrId);
		customerDomainObject.addAssociation(otoAddress);
		customerDomainObject.setStartXPosition(20);
		customerDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		return customerDomainObject;
	}

	/**
	 * Create the supplier domain object
	 * @param mappedSuperclass
	 * @return the new domain object
	 */
	private static DomainObject createSupplierDomainObject(DomainObject mappedSuperclass) {
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);
		final var attActive = new DomainAttribute("active", DomainAttribute.TYPE_BOOLEAN, false, false);

		final var supplierDomainObject = new DomainObject(DOMAIN_OBJECT_SUPPLIER);
		supplierDomainObject.setNamespace(Project.ADDITIONAL_NAMESPACE);
		supplierDomainObject.setParentDomainObject(mappedSuperclass);
		supplierDomainObject.addAttribute(attrName);
		supplierDomainObject.addAttribute(attActive);
		supplierDomainObject.setStartXPosition(300);
		supplierDomainObject.setStartYPosition(20);

		return supplierDomainObject;
	}

	/**
	 * Create the mapped superclass
	 * @return the new domain object
	 */
	private static DomainObject createMappedSuperClass() {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrVersion = new DomainAttribute("version", DomainAttribute.TYPE_LONG, false, false);

		final var baseEntity = new DomainObject(DOMAIN_OBJECT_BASE_ENTITY);
		baseEntity.setMappedSuperClass(true);
		baseEntity.addAttribute(attrId);
		baseEntity.addAttribute(attrVersion);
		baseEntity.setStartXPosition(20);
		baseEntity.setStartYPosition(20);

		return baseEntity;
	}

	/**
	 * Create the department domain object
	 * @param officeDomainObject
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createDepartmentDomainObject(DomainObject officeDomainObject, Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);

		final var departmentDomainObject = new DomainObject(DOMAIN_OBJECT_DEPARTMENT);
		departmentDomainObject.addAttribute(attrName);
		departmentDomainObject.addAttribute(attrId);
		departmentDomainObject.setStartXPosition(20);
		departmentDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		if (officeDomainObject != null) {
			final var otmOffices = new DomainAssociation("offices", officeDomainObject, AssociationType.ONE_TO_MANY, false);
			departmentDomainObject.addAssociation(otmOffices);
		}

		return departmentDomainObject;
	}

	/**
	 * Create the office domain object
	 * @param startYPosition
	 * @return the new domain object
	 */
	private static DomainObject createOfficeDomainObject(Integer startYPosition) {
		final var attrId = new DomainAttribute("id", DomainAttribute.TYPE_LONG, true, false);
		final var attrName = new DomainAttribute("name", DomainAttribute.TYPE_STRING, false, true);

		final var officeDomainObject = new DomainObject(DOMAIN_OBJECT_OFFICE);
		officeDomainObject.addAttribute(attrName);
		officeDomainObject.addAttribute(attrId);
		officeDomainObject.setStartXPosition(500);
		officeDomainObject.setStartYPosition(startYPosition != null ? startYPosition : DEFAULT_START_Y_POS);

		return officeDomainObject;
	}

	/**
	 * Create a new {@link DataSource} with a random schema and database name. The JDBC driver list is loaded from the default
	 * location in the user home directory.
	 * @param databaseVendor
	 * @return a new {@link DataSource}
	 */
	private static DataSource initDataSource(DatabaseVendor databaseVendor) {
		final String randomId = UUID.randomUUID().toString().substring(0, 7);
		final var schemaName = DB_SCHEMA_PREFIX + randomId;
		final var databaseName = DB_NAME_PREFIX + randomId;
		final var driverList = new ArrayList<String>();
		final String connectionURL;
		final String driverName;

		if (databaseVendor == DatabaseVendor.MYSQL) {
			connectionURL = DB_URL_MYSQL;
			driverName = DB_DRIVER_NAME_MYSQL;
		}
		else {
			connectionURL = DB_URL_POSTGRESQL + databaseName;
			driverName = DB_DRIVER_NAME_POSTGRESQL;
		}

		// Search for the drivers
		final var userHomeDirectory = new File(System.getProperty("user.home"));
		final var libDirectory = new File(userHomeDirectory, LOCAL_JDBC_LIB_PATH + databaseVendor.name().toLowerCase());

		if (libDirectory.exists() && libDirectory.isDirectory())
			for (final var file : libDirectory.listFiles())
				driverList.add(file.getAbsolutePath());

		return new DataSource(driverName, connectionURL, schemaName, DB_USER_NAME, DB_PASSWORD, driverList, databaseVendor,
				databaseName);
	}

}
