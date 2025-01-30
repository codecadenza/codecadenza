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
package net.codecadenza.eclipse.model.integration.impl;

import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationFactory;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RMIIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model <b>Factory</b>.
 * @generated
 */
public class IntegrationFactoryImpl extends EFactoryImpl implements IntegrationFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static IntegrationFactory init() {
		try {
			final var theIntegrationFactory = (IntegrationFactory) EPackage.Registry.INSTANCE.getEFactory(IntegrationPackage.eNS_URI);

			if (theIntegrationFactory != null)
				return theIntegrationFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new IntegrationFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case IntegrationPackage.SOAP_INTEGRATION_BEAN -> createSOAPIntegrationBean();
			case IntegrationPackage.REST_INTEGRATION_BEAN -> createRESTIntegrationBean();
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN -> createAbstractIntegrationBean();
			case IntegrationPackage.SOAP_INTEGRATION_METHOD -> createSOAPIntegrationMethod();
			case IntegrationPackage.REST_INTEGRATION_METHOD -> createRESTIntegrationMethod();
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD -> createAbstractIntegrationMethod();
			case IntegrationPackage.RMI_INTEGRATION_METHOD -> createRMIIntegrationMethod();
			case IntegrationPackage.RMI_INTEGRATION_BEAN -> createRMIIntegrationBean();
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD -> createKafkaIntegrationMethod();
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN -> createKafkaIntegrationBean();
			case IntegrationPackage.JMS_INTEGRATION_METHOD -> createJMSIntegrationMethod();
			case IntegrationPackage.JMS_INTEGRATION_BEAN -> createJMSIntegrationBean();
			case IntegrationPackage.JMS_RESOURCE -> createJMSResource();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#createFromString(org.eclipse.emf.ecore.EDataType, java.lang.String)
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		return switch (eDataType.getClassifierID()) {
			case IntegrationPackage.MEDIA_TYPE_ENUMERATION -> createMediaTypeEnumerationFromString(eDataType, initialValue);
			case IntegrationPackage.HTTP_METHOD_ENUMERATION -> createHttpMethodEnumerationFromString(eDataType, initialValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#convertToString(org.eclipse.emf.ecore.EDataType, java.lang.Object)
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		return switch (eDataType.getClassifierID()) {
			case IntegrationPackage.MEDIA_TYPE_ENUMERATION -> convertMediaTypeEnumerationToString(eDataType, instanceValue);
			case IntegrationPackage.HTTP_METHOD_ENUMERATION -> convertHttpMethodEnumerationToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createSOAPIntegrationBean()
	 * @generated
	 */
	@Override
	public SOAPIntegrationBean createSOAPIntegrationBean() {
		return new SOAPIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createRESTIntegrationBean()
	 * @generated
	 */
	@Override
	public RESTIntegrationBean createRESTIntegrationBean() {
		return new RESTIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createAbstractIntegrationBean()
	 * @generated
	 */
	@Override
	public AbstractIntegrationBean createAbstractIntegrationBean() {
		return new AbstractIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createSOAPIntegrationMethod()
	 * @generated
	 */
	@Override
	public SOAPIntegrationMethod createSOAPIntegrationMethod() {
		return new SOAPIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createRESTIntegrationMethod()
	 * @generated
	 */
	@Override
	public RESTIntegrationMethod createRESTIntegrationMethod() {
		return new RESTIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createAbstractIntegrationMethod()
	 * @generated
	 */
	@Override
	public AbstractIntegrationMethod createAbstractIntegrationMethod() {
		return new AbstractIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createRMIIntegrationMethod()
	 * @generated
	 */
	@Override
	public RMIIntegrationMethod createRMIIntegrationMethod() {
		return new RMIIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createRMIIntegrationBean()
	 * @generated
	 */
	@Override
	public RMIIntegrationBean createRMIIntegrationBean() {
		return new RMIIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createKafkaIntegrationBean()
	 * @generated
	 */
	@Override
	public KafkaIntegrationBean createKafkaIntegrationBean() {
		return new KafkaIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createKafkaIntegrationMethod()
	 * @generated
	 */
	@Override
	public KafkaIntegrationMethod createKafkaIntegrationMethod() {
		return new KafkaIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createJMSIntegrationMethod()
	 * @generated
	 */
	@Override
	public JMSIntegrationMethod createJMSIntegrationMethod() {
		return new JMSIntegrationMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createJMSIntegrationBean()
	 * @generated
	 */
	@Override
	public JMSIntegrationBean createJMSIntegrationBean() {
		return new JMSIntegrationBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#createJMSResource()
	 * @generated
	 */
	@Override
	public JMSResource createJMSResource() {
		return new JMSResourceImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public MediaTypeEnumeration createMediaTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final MediaTypeEnumeration result = MediaTypeEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertMediaTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public HttpMethodEnumeration createHttpMethodEnumerationFromString(EDataType eDataType, String initialValue) {
		final HttpMethodEnumeration result = HttpMethodEnumeration.get(initialValue);

		if (result == null)
			throw new IllegalArgumentException(
					"The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");

		return result;
	}

	/**
	 * @param eDataType
	 * @param instanceValue
	 * @return the String value
	 * @generated
	 */
	@SuppressWarnings("unused")
	public String convertHttpMethodEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationFactory#getIntegrationPackage()
	 * @generated
	 */
	@Override
	public IntegrationPackage getIntegrationPackage() {
		return (IntegrationPackage) getEPackage();
	}

	/**
	 * @return the integration package
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static IntegrationPackage getPackage() {
		return IntegrationPackage.eINSTANCE;
	}

}
