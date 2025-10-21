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
package net.codecadenza.eclipse.model.project;

import java.util.List;
import org.eclipse.emf.common.util.Enumerator;

/**
 * A representation of the literals of the enumeration '<em><b>Build Artifact Type</b></em>', and utility methods for working with
 * them.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage#getBuildArtifactType()
 * @model
 * @generated
 */
public enum BuildArtifactType implements Enumerator {
	/**
	 * The '<em><b>DOMAIN</b></em>' literal object
	 * @see #DOMAIN_VALUE
	 * @generated
	 * @ordered
	 */
	DOMAIN(0, "DOMAIN", "DOMAIN"),

	/**
	 * The '<em><b>REPOSITORY</b></em>' literal object
	 * @see #REPOSITORY_VALUE
	 * @generated
	 * @ordered
	 */
	REPOSITORY(1, "REPOSITORY", "REPOSITORY"),

	/**
	 * The '<em><b>BOUNDARY</b></em>' literal object
	 * @see #BOUNDARY_VALUE
	 * @generated
	 * @ordered
	 */
	BOUNDARY(2, "BOUNDARY", "BOUNDARY"),

	/**
	 * The '<em><b>FACADE</b></em>' literal object
	 * @see #FACADE_VALUE
	 * @generated
	 * @ordered
	 */
	FACADE(3, "FACADE", "FACADE"),

	/**
	 * The '<em><b>SERVICE</b></em>' literal object
	 * @see #SERVICE_VALUE
	 * @generated
	 * @ordered
	 */
	SERVICE(4, "SERVICE", "SERVICE"),

	/**
	 * The '<em><b>CLIENT INTERFACE</b></em>' literal object
	 * @see #CLIENT_INTERFACE_VALUE
	 * @generated
	 * @ordered
	 */
	CLIENT_INTERFACE(5, "CLIENT_INTERFACE", "CLIENT_INTERFACE"),

	/**
	 * The '<em><b>SERVER</b></em>' literal object
	 * @see #SERVER_VALUE
	 * @generated
	 * @ordered
	 */
	SERVER(6, "SERVER", "SERVER"),

	/**
	 * The '<em><b>GUI</b></em>' literal object
	 * @see #GUI_VALUE
	 * @generated
	 * @ordered
	 */
	GUI(7, "GUI", "GUI"),

	/**
	 * The '<em><b>MASTER</b></em>' literal object
	 * @see #MASTER_VALUE
	 * @generated
	 * @ordered
	 */
	MASTER(8, "MASTER", "MASTER"),

	/**
	 * The '<em><b>DTO</b></em>' literal object
	 * @see #DTO_VALUE
	 * @generated
	 * @ordered
	 */
	DTO(9, "DTO", "DTO"),

	/**
	 * The '<em><b>DATA EXCHANGE</b></em>' literal object
	 * @see #DATA_EXCHANGE_VALUE
	 * @generated
	 * @ordered
	 */
	DATA_EXCHANGE(10, "DATA_EXCHANGE", "DATA_EXCHANGE"),

	/**
	 * The '<em><b>INTEGRATION IMP SOAP</b></em>' literal object
	 * @see #INTEGRATION_IMP_SOAP_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_IMP_SOAP(11, "INTEGRATION_IMP_SOAP", "INTEGRATION_IMP_SOAP"),

	/**
	 * The '<em><b>INTEGRATION SEI SOAP</b></em>' literal object
	 * @see #INTEGRATION_SEI_SOAP_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_SEI_SOAP(12, "INTEGRATION_SEI_SOAP", "INTEGRATION_SEI_SOAP"),

	/**
	 * The '<em><b>INTEGRATION CLIENT SOAP</b></em>' literal object
	 * @see #INTEGRATION_CLIENT_SOAP_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_CLIENT_SOAP(13, "INTEGRATION_CLIENT_SOAP", "INTEGRATION_CLIENT_SOAP"),

	/**
	 * The '<em><b>INTEGRATION IMP REST</b></em>' literal object
	 * @see #INTEGRATION_IMP_REST_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_IMP_REST(14, "INTEGRATION_IMP_REST", "INTEGRATION_IMP_REST"),

	/**
	 * The '<em><b>INTEGRATION SEI REST</b></em>' literal object
	 * @see #INTEGRATION_SEI_REST_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_SEI_REST(15, "INTEGRATION_SEI_REST", "INTEGRATION_SEI_REST"),

	/**
	 * The '<em><b>INTEGRATION CLIENT REST</b></em>' literal object
	 * @see #INTEGRATION_CLIENT_REST_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_CLIENT_REST(16, "INTEGRATION_CLIENT_REST", "INTEGRATION_CLIENT_REST"),

	/**
	 * The '<em><b>INTEGRATION IMP RMI</b></em>' literal object
	 * @see #INTEGRATION_IMP_RMI_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_IMP_RMI(17, "INTEGRATION_IMP_RMI", "INTEGRATION_IMP_RMI"),

	/**
	 * The '<em><b>INTEGRATION SEI RMI</b></em>' literal object
	 * @see #INTEGRATION_SEI_RMI_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_SEI_RMI(18, "INTEGRATION_SEI_RMI", "INTEGRATION_SEI_RMI"),

	/**
	 * The '<em><b>INTEGRATION CLIENT RMI</b></em>' literal object
	 * @see #INTEGRATION_CLIENT_RMI_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_CLIENT_RMI(19, "INTEGRATION_CLIENT_RMI", "INTEGRATION_CLIENT_RMI"),

	/**
	 * The '<em><b>SELENIUM TEST</b></em>' literal object
	 * @see #SELENIUM_TEST_VALUE
	 * @generated
	 * @ordered
	 */
	SELENIUM_TEST(20, "SELENIUM_TEST", "SELENIUM_TEST"),

	/**
	 * The '<em><b>SHARED</b></em>' literal object
	 * @see #SHARED_VALUE
	 * @generated
	 * @ordered
	 */
	SHARED(21, "SHARED", "SHARED"),

	/**
	 * The '<em><b>INTEGRATION IMP KAFKA</b></em>' literal object
	 * @see #INTEGRATION_IMP_KAFKA_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_IMP_KAFKA(22, "INTEGRATION_IMP_KAFKA", "INTEGRATION_IMP_KAFKA"),

	/**
	 * The '<em><b>INTEGRATION SEI KAFKA</b></em>' literal object
	 * @see #INTEGRATION_SEI_KAFKA_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_SEI_KAFKA(23, "INTEGRATION_SEI_KAFKA", "INTEGRATION_SEI_KAFKA"),

	/**
	 * The '<em><b>INTEGRATION CLIENT KAFKA</b></em>' literal object
	 * @see #INTEGRATION_CLIENT_KAFKA_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_CLIENT_KAFKA(24, "INTEGRATION_CLIENT_KAFKA", "INTEGRATION_CLIENT_KAFKA"),

	/**
	 * The '<em><b>INTEGRATION IMP JMS</b></em>' literal object
	 * @see #INTEGRATION_IMP_JMS_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_IMP_JMS(25, "INTEGRATION_IMP_JMS", "INTEGRATION_IMP_JMS"),

	/**
	 * The '<em><b>INTEGRATION SEI JMS</b></em>' literal object
	 * @see #INTEGRATION_SEI_JMS_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_SEI_JMS(26, "INTEGRATION_SEI_JMS", "INTEGRATION_SEI_JMS"),

	/**
	 * The '<em><b>INTEGRATION CLIENT JMS</b></em>' literal object
	 * @see #INTEGRATION_CLIENT_JMS_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_CLIENT_JMS(27, "INTEGRATION_CLIENT_JMS", "INTEGRATION_CLIENT_JMS"),

	/**
	 * The '<em><b>INTEGRATION TEST SOAP</b></em>' literal object
	 * @see #INTEGRATION_TEST_SOAP_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_TEST_SOAP(28, "INTEGRATION_TEST_SOAP", "INTEGRATION_TEST_SOAP"),

	/**
	 * The '<em><b>INTEGRATION TEST REST</b></em>' literal object
	 * @see #INTEGRATION_TEST_REST_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_TEST_REST(29, "INTEGRATION_TEST_REST", "INTEGRATION_TEST_REST"),

	/**
	 * The '<em><b>INTEGRATION TEST RMI</b></em>' literal object
	 * @see #INTEGRATION_TEST_RMI_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_TEST_RMI(30, "INTEGRATION_TEST_RMI", "INTEGRATION_TEST_RMI"),

	/**
	 * The '<em><b>INTEGRATION TEST KAFKA</b></em>' literal object
	 * @see #INTEGRATION_TEST_KAFKA_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_TEST_KAFKA(31, "INTEGRATION_TEST_KAFKA", "INTEGRATION_TEST_KAFKA"),

	/**
	 * The '<em><b>INTEGRATION TEST JMS</b></em>' literal object
	 * @see #INTEGRATION_TEST_JMS_VALUE
	 * @generated
	 * @ordered
	 */
	INTEGRATION_TEST_JMS(32, "INTEGRATION_TEST_JMS", "INTEGRATION_TEST_JMS");

	/**
	 * The '<em><b>DOMAIN</b></em>' literal value
	 * @see #DOMAIN
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DOMAIN_VALUE = 0;

	/**
	 * The '<em><b>REPOSITORY</b></em>' literal value
	 * @see #REPOSITORY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int REPOSITORY_VALUE = 1;

	/**
	 * The '<em><b>BOUNDARY</b></em>' literal value
	 * @see #BOUNDARY
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int BOUNDARY_VALUE = 2;

	/**
	 * The '<em><b>FACADE</b></em>' literal value
	 * @see #FACADE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int FACADE_VALUE = 3;

	/**
	 * The '<em><b>SERVICE</b></em>' literal value
	 * @see #SERVICE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SERVICE_VALUE = 4;

	/**
	 * The '<em><b>CLIENT INTERFACE</b></em>' literal value
	 * @see #CLIENT_INTERFACE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int CLIENT_INTERFACE_VALUE = 5;

	/**
	 * The '<em><b>SERVER</b></em>' literal value
	 * @see #SERVER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SERVER_VALUE = 6;

	/**
	 * The '<em><b>GUI</b></em>' literal value
	 * @see #GUI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int GUI_VALUE = 7;

	/**
	 * The '<em><b>MASTER</b></em>' literal value
	 * @see #MASTER
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int MASTER_VALUE = 8;

	/**
	 * The '<em><b>DTO</b></em>' literal value
	 * @see #DTO
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DTO_VALUE = 9;

	/**
	 * The '<em><b>DATA EXCHANGE</b></em>' literal value
	 * @see #DATA_EXCHANGE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int DATA_EXCHANGE_VALUE = 10;

	/**
	 * The '<em><b>INTEGRATION IMP SOAP</b></em>' literal value
	 * @see #INTEGRATION_IMP_SOAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_IMP_SOAP_VALUE = 11;

	/**
	 * The '<em><b>INTEGRATION SEI SOAP</b></em>' literal value
	 * @see #INTEGRATION_SEI_SOAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_SEI_SOAP_VALUE = 12;

	/**
	 * The '<em><b>INTEGRATION CLIENT SOAP</b></em>' literal value
	 * @see #INTEGRATION_CLIENT_SOAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_CLIENT_SOAP_VALUE = 13;

	/**
	 * The '<em><b>INTEGRATION IMP REST</b></em>' literal value
	 * @see #INTEGRATION_IMP_REST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_IMP_REST_VALUE = 14;

	/**
	 * The '<em><b>INTEGRATION SEI REST</b></em>' literal value
	 * @see #INTEGRATION_SEI_REST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_SEI_REST_VALUE = 15;

	/**
	 * The '<em><b>INTEGRATION CLIENT REST</b></em>' literal value
	 * @see #INTEGRATION_CLIENT_REST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_CLIENT_REST_VALUE = 16;

	/**
	 * The '<em><b>INTEGRATION IMP RMI</b></em>' literal value
	 * @see #INTEGRATION_IMP_RMI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_IMP_RMI_VALUE = 17;

	/**
	 * The '<em><b>INTEGRATION SEI RMI</b></em>' literal value
	 * @see #INTEGRATION_SEI_RMI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_SEI_RMI_VALUE = 18;

	/**
	 * The '<em><b>INTEGRATION CLIENT RMI</b></em>' literal value
	 * @see #INTEGRATION_CLIENT_RMI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_CLIENT_RMI_VALUE = 19;

	/**
	 * The '<em><b>SELENIUM TEST</b></em>' literal value
	 * @see #SELENIUM_TEST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SELENIUM_TEST_VALUE = 20;

	/**
	 * The '<em><b>SHARED</b></em>' literal value
	 * @see #SHARED
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int SHARED_VALUE = 21;

	/**
	 * The '<em><b>INTEGRATION IMP KAFKA</b></em>' literal value
	 * @see #INTEGRATION_IMP_KAFKA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_IMP_KAFKA_VALUE = 22;

	/**
	 * The '<em><b>INTEGRATION SEI KAFKA</b></em>' literal value
	 * @see #INTEGRATION_SEI_KAFKA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_SEI_KAFKA_VALUE = 23;

	/**
	 * The '<em><b>INTEGRATION CLIENT KAFKA</b></em>' literal value
	 * @see #INTEGRATION_CLIENT_KAFKA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_CLIENT_KAFKA_VALUE = 24;

	/**
	 * The '<em><b>INTEGRATION IMP JMS</b></em>' literal value
	 * @see #INTEGRATION_IMP_JMS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_IMP_JMS_VALUE = 25;

	/**
	 * The '<em><b>INTEGRATION SEI JMS</b></em>' literal value
	 * @see #INTEGRATION_SEI_JMS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_SEI_JMS_VALUE = 26;

	/**
	 * The '<em><b>INTEGRATION CLIENT JMS</b></em>' literal value
	 * @see #INTEGRATION_CLIENT_JMS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_CLIENT_JMS_VALUE = 27;

	/**
	 * The '<em><b>INTEGRATION TEST SOAP</b></em>' literal value
	 * @see #INTEGRATION_TEST_SOAP
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_TEST_SOAP_VALUE = 28;

	/**
	 * The '<em><b>INTEGRATION TEST REST</b></em>' literal value
	 * @see #INTEGRATION_TEST_REST
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_TEST_REST_VALUE = 29;

	/**
	 * The '<em><b>INTEGRATION TEST RMI</b></em>' literal value
	 * @see #INTEGRATION_TEST_RMI
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_TEST_RMI_VALUE = 30;

	/**
	 * The '<em><b>INTEGRATION TEST KAFKA</b></em>' literal value
	 * @see #INTEGRATION_TEST_KAFKA
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_TEST_KAFKA_VALUE = 31;

	/**
	 * The '<em><b>INTEGRATION TEST JMS</b></em>' literal value
	 * @see #INTEGRATION_TEST_JMS
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int INTEGRATION_TEST_JMS_VALUE = 32;

	/**
	 * An array of all the '<em><b>Build Artifact Type</b></em>' enumerators
	 * @generated
	 */
	private static final BuildArtifactType[] VALUES_ARRAY = { DOMAIN, REPOSITORY, BOUNDARY, FACADE, SERVICE, CLIENT_INTERFACE,
			SERVER, GUI, MASTER, DTO, DATA_EXCHANGE, INTEGRATION_IMP_SOAP, INTEGRATION_SEI_SOAP, INTEGRATION_CLIENT_SOAP,
			INTEGRATION_IMP_REST, INTEGRATION_SEI_REST, INTEGRATION_CLIENT_REST, INTEGRATION_IMP_RMI, INTEGRATION_SEI_RMI,
			INTEGRATION_CLIENT_RMI, SELENIUM_TEST, SHARED, INTEGRATION_IMP_KAFKA, INTEGRATION_SEI_KAFKA, INTEGRATION_CLIENT_KAFKA,
			INTEGRATION_IMP_JMS, INTEGRATION_SEI_JMS, INTEGRATION_CLIENT_JMS, INTEGRATION_TEST_SOAP, INTEGRATION_TEST_REST,
			INTEGRATION_TEST_RMI, INTEGRATION_TEST_KAFKA, INTEGRATION_TEST_JMS };

	/**
	 * A public read-only list of all the '<em><b>Build Artifact Type</b></em>' enumerators
	 * @generated
	 */
	public static final List<BuildArtifactType> VALUES = List.of(VALUES_ARRAY);

	/**
	 * @param literal
	 * @return the '<em><b>Build Artifact Type</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static BuildArtifactType get(String literal) {
		return VALUES.stream().filter(value -> value.toString().equals(literal)).findFirst().orElse(null);
	}

	/**
	 * @param name
	 * @return the '<em><b>Build Artifact Type</b></em>' literal with the specified literal value
	 * @generated
	 */
	public static BuildArtifactType getByName(String name) {
		return VALUES.stream().filter(value -> value.getName().equals(name)).findFirst().orElse(null);
	}

	/**
	 * @param value
	 * @return the '<em><b>Build Artifact Type</b></em>' literal with the specified integer value
	 * @generated
	 */
	public static BuildArtifactType get(int value) {
		switch (value) {
			case DOMAIN_VALUE:
				return DOMAIN;
			case REPOSITORY_VALUE:
				return REPOSITORY;
			case BOUNDARY_VALUE:
				return BOUNDARY;
			case FACADE_VALUE:
				return FACADE;
			case SERVICE_VALUE:
				return SERVICE;
			case CLIENT_INTERFACE_VALUE:
				return CLIENT_INTERFACE;
			case SERVER_VALUE:
				return SERVER;
			case GUI_VALUE:
				return GUI;
			case MASTER_VALUE:
				return MASTER;
			case DTO_VALUE:
				return DTO;
			case DATA_EXCHANGE_VALUE:
				return DATA_EXCHANGE;
			case INTEGRATION_IMP_SOAP_VALUE:
				return INTEGRATION_IMP_SOAP;
			case INTEGRATION_SEI_SOAP_VALUE:
				return INTEGRATION_SEI_SOAP;
			case INTEGRATION_CLIENT_SOAP_VALUE:
				return INTEGRATION_CLIENT_SOAP;
			case INTEGRATION_IMP_REST_VALUE:
				return INTEGRATION_IMP_REST;
			case INTEGRATION_SEI_REST_VALUE:
				return INTEGRATION_SEI_REST;
			case INTEGRATION_CLIENT_REST_VALUE:
				return INTEGRATION_CLIENT_REST;
			case INTEGRATION_IMP_RMI_VALUE:
				return INTEGRATION_IMP_RMI;
			case INTEGRATION_SEI_RMI_VALUE:
				return INTEGRATION_SEI_RMI;
			case INTEGRATION_CLIENT_RMI_VALUE:
				return INTEGRATION_CLIENT_RMI;
			case SELENIUM_TEST_VALUE:
				return SELENIUM_TEST;
			case SHARED_VALUE:
				return SHARED;
			case INTEGRATION_IMP_KAFKA_VALUE:
				return INTEGRATION_IMP_KAFKA;
			case INTEGRATION_SEI_KAFKA_VALUE:
				return INTEGRATION_SEI_KAFKA;
			case INTEGRATION_CLIENT_KAFKA_VALUE:
				return INTEGRATION_CLIENT_KAFKA;
			case INTEGRATION_IMP_JMS_VALUE:
				return INTEGRATION_IMP_JMS;
			case INTEGRATION_SEI_JMS_VALUE:
				return INTEGRATION_SEI_JMS;
			case INTEGRATION_CLIENT_JMS_VALUE:
				return INTEGRATION_CLIENT_JMS;
			case INTEGRATION_TEST_SOAP_VALUE:
				return INTEGRATION_TEST_SOAP;
			case INTEGRATION_TEST_REST_VALUE:
				return INTEGRATION_TEST_REST;
			case INTEGRATION_TEST_RMI_VALUE:
				return INTEGRATION_TEST_RMI;
			case INTEGRATION_TEST_KAFKA_VALUE:
				return INTEGRATION_TEST_KAFKA;
			case INTEGRATION_TEST_JMS_VALUE:
				return INTEGRATION_TEST_JMS;
		}

		return null;
	}

	/**
	 * @generated
	 */
	private final int value;

	/**
	 * @generated
	 */
	private final String name;

	/**
	 * @generated
	 */
	private final String literal;

	/**
	 * Constructor
	 * @param value
	 * @param name
	 * @param literal
	 * @generated
	 */
	BuildArtifactType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getValue()
	 * @generated
	 */
	@Override
	public int getValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.util.Enumerator#getLiteral()
	 * @generated
	 */
	@Override
	public String getLiteral() {
		return literal;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}

}
