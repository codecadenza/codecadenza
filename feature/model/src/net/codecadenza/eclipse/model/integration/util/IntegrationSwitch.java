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
package net.codecadenza.eclipse.model.integration.util;

import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RMIIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage
 * @generated
 */
public class IntegrationSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static IntegrationPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public IntegrationSwitch() {
		if (modelPackage == null)
			modelPackage = IntegrationPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case IntegrationPackage.SOAP_INTEGRATION_BEAN: {
				final var soapIntegrationBean = (SOAPIntegrationBean) theEObject;
				T result = caseSOAPIntegrationBean(soapIntegrationBean);

				if (result == null)
					result = caseAbstractIntegrationBean(soapIntegrationBean);

				if (result == null)
					result = caseServiceBean(soapIntegrationBean);

				if (result == null)
					result = caseJavaType(soapIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.REST_INTEGRATION_BEAN: {
				final var restIntegrationBean = (RESTIntegrationBean) theEObject;
				T result = caseRESTIntegrationBean(restIntegrationBean);

				if (result == null)
					result = caseAbstractIntegrationBean(restIntegrationBean);

				if (result == null)
					result = caseServiceBean(restIntegrationBean);

				if (result == null)
					result = caseJavaType(restIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.ABSTRACT_INTEGRATION_BEAN: {
				final var abstractIntegrationBean = (AbstractIntegrationBean) theEObject;
				T result = caseAbstractIntegrationBean(abstractIntegrationBean);

				if (result == null)
					result = caseServiceBean(abstractIntegrationBean);

				if (result == null)
					result = caseJavaType(abstractIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.SOAP_INTEGRATION_METHOD: {
				final var soapIntegrationMethod = (SOAPIntegrationMethod) theEObject;
				T result = caseSOAPIntegrationMethod(soapIntegrationMethod);

				if (result == null)
					result = caseAbstractIntegrationMethod(soapIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(soapIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(soapIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.REST_INTEGRATION_METHOD: {
				final var restIntegrationMethod = (RESTIntegrationMethod) theEObject;
				T result = caseRESTIntegrationMethod(restIntegrationMethod);

				if (result == null)
					result = caseAbstractIntegrationMethod(restIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(restIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(restIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.ABSTRACT_INTEGRATION_METHOD: {
				final var abstractIntegrationMethod = (AbstractIntegrationMethod) theEObject;
				T result = caseAbstractIntegrationMethod(abstractIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(abstractIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(abstractIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.RMI_INTEGRATION_METHOD: {
				final var rmiIntegrationMethod = (RMIIntegrationMethod) theEObject;
				T result = caseRMIIntegrationMethod(rmiIntegrationMethod);

				if (result == null)
					result = caseAbstractIntegrationMethod(rmiIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(rmiIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(rmiIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.RMI_INTEGRATION_BEAN: {
				final var rmiIntegrationBean = (RMIIntegrationBean) theEObject;
				T result = caseRMIIntegrationBean(rmiIntegrationBean);

				if (result == null)
					result = caseAbstractIntegrationBean(rmiIntegrationBean);

				if (result == null)
					result = caseServiceBean(rmiIntegrationBean);

				if (result == null)
					result = caseJavaType(rmiIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.KAFKA_INTEGRATION_METHOD: {
				final var kafkaIntegrationMethod = (KafkaIntegrationMethod) theEObject;
				T result = caseKafkaIntegrationMethod(kafkaIntegrationMethod);

				if (result == null)
					result = caseAbstractIntegrationMethod(kafkaIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(kafkaIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(kafkaIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.KAFKA_INTEGRATION_BEAN: {
				final var kafkaIntegrationBean = (KafkaIntegrationBean) theEObject;
				T result = caseKafkaIntegrationBean(kafkaIntegrationBean);

				if (result == null)
					result = caseAbstractIntegrationBean(kafkaIntegrationBean);

				if (result == null)
					result = caseServiceBean(kafkaIntegrationBean);

				if (result == null)
					result = caseJavaType(kafkaIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.JMS_INTEGRATION_METHOD: {
				final JMSIntegrationMethod jmsIntegrationMethod = (JMSIntegrationMethod) theEObject;
				T result = caseJMSIntegrationMethod(jmsIntegrationMethod);

				if (result == null)
					result = caseAbstractIntegrationMethod(jmsIntegrationMethod);

				if (result == null)
					result = caseServiceMethod(jmsIntegrationMethod);

				if (result == null)
					result = caseJavaMethod(jmsIntegrationMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.JMS_INTEGRATION_BEAN: {
				final JMSIntegrationBean jmsIntegrationBean = (JMSIntegrationBean) theEObject;
				T result = caseJMSIntegrationBean(jmsIntegrationBean);

				if (result == null)
					result = caseAbstractIntegrationBean(jmsIntegrationBean);

				if (result == null)
					result = caseServiceBean(jmsIntegrationBean);

				if (result == null)
					result = caseJavaType(jmsIntegrationBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case IntegrationPackage.JMS_RESOURCE: {
				final JMSResource jmsResource = (JMSResource) theEObject;
				T result = caseJMSResource(jmsResource);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>SOAP Integration Bean</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>SOAP Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseSOAPIntegrationBean(SOAPIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>REST Integration Bean</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>REST Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseRESTIntegrationBean(RESTIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>RMI Integration Bean</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>RMI Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseRMIIntegrationBean(RMIIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Kafka Integration Bean</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Kafka Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseKafkaIntegrationBean(KafkaIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>JMS Integration Bean</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>JMS Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJMSIntegrationBean(JMSIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Abstract Integration Bean</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Abstract Integration Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAbstractIntegrationBean(AbstractIntegrationBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>SOAP Integration Method</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>SOAP Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseSOAPIntegrationMethod(SOAPIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>REST Integration Method</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>REST Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseRESTIntegrationMethod(RESTIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>RMI Integration Method</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>RMI Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseRMIIntegrationMethod(RMIIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Kafka Integration Method</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Kafka Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseKafkaIntegrationMethod(KafkaIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>JMS Integration Method</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>JMS Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJMSIntegrationMethod(JMSIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Abstract Integration Method</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Abstract Integration Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAbstractIntegrationMethod(AbstractIntegrationMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>JMS Resource</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>JMS Resource</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJMSResource(JMSResource object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Type</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Type</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaType(JavaType object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Bean</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceBean(ServiceBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Method</em>'. This implementation returns null; returning
	 * a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaMethod(JavaMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Method</em>'. This implementation returns null; returning
	 * a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceMethod(ServiceMethod object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
