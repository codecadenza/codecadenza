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
package net.codecadenza.eclipse.model.exchange.util;

import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMode;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
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
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage
 * @generated
 */
public class ExchangeSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ExchangePackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public ExchangeSwitch() {
		if (modelPackage == null)
			modelPackage = ExchangePackage.eINSTANCE;
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
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN: {
				final var dataExchangeServiceBean = (DataExchangeServiceBean) theEObject;
				T result = caseDataExchangeServiceBean(dataExchangeServiceBean);

				if (result == null)
					result = caseServiceBean(dataExchangeServiceBean);

				if (result == null)
					result = caseJavaType(dataExchangeServiceBean);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.DATA_EXCHANGE_METHOD: {
				final var dataExchangeMethod = (DataExchangeMethod) theEObject;
				T result = caseDataExchangeMethod(dataExchangeMethod);

				if (result == null)
					result = caseServiceMethod(dataExchangeMethod);

				if (result == null)
					result = caseJavaMethod(dataExchangeMethod);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.DATA_EXCHANGE_MODE: {
				final var dataExchangeMode = (DataExchangeMode) theEObject;
				T result = caseDataExchangeMode(dataExchangeMode);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.STRING_EXCHANGE_MODE: {
				final var stringExchangeMode = (StringExchangeMode) theEObject;
				T result = caseStringExchangeMode(stringExchangeMode);

				if (result == null)
					result = caseDataExchangeMode(stringExchangeMode);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.FILE_EXCHANGE_MODE: {
				final var fileExchangeMode = (FileExchangeMode) theEObject;
				T result = caseFileExchangeMode(fileExchangeMode);

				if (result == null)
					result = caseDataExchangeMode(fileExchangeMode);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.DIRECT_EXCHANGE_MODE: {
				final var directExchangeMode = (DirectExchangeMode) theEObject;
				T result = caseDirectExchangeMode(directExchangeMode);

				if (result == null)
					result = caseDataExchangeMode(directExchangeMode);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.DATA_EXCHANGE_ELEMENT: {
				final var dataExchangeElement = (DataExchangeElement) theEObject;
				T result = caseDataExchangeElement(dataExchangeElement);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE: {
				final var dataExchangeAttribute = (DataExchangeAttribute) theEObject;
				T result = caseDataExchangeAttribute(dataExchangeAttribute);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.VALUE_LIST_ENTRY: {
				final var valueListEntry = (ValueListEntry) theEObject;
				T result = caseValueListEntry(valueListEntry);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT: {
				final var exchangeMappingObject = (ExchangeMappingObject) theEObject;
				T result = caseExchangeMappingObject(exchangeMappingObject);

				if (result == null)
					result = caseMappingObject(exchangeMappingObject);

				if (result == null)
					result = caseJavaType(exchangeMappingObject);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE: {
				final var exchangeMappingAttribute = (ExchangeMappingAttribute) theEObject;
				T result = caseExchangeMappingAttribute(exchangeMappingAttribute);

				if (result == null)
					result = caseMappingAttribute(exchangeMappingAttribute);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.FILTER_METHOD_PARAMETER: {
				final var filterMethodParameter = (FilterMethodParameter) theEObject;
				T result = caseFilterMethodParameter(filterMethodParameter);

				if (result == null)
					result = caseMethodParameter(filterMethodParameter);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ExchangePackage.ASSOCIATION_CONTROLLER: {
				final var associationController = (AssociationController) theEObject;
				T result = caseAssociationController(associationController);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Data Exchange Service Bean</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Data Exchange Service Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDataExchangeServiceBean(DataExchangeServiceBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Data Exchange Method</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Data Exchange Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDataExchangeMethod(DataExchangeMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Data Exchange Mode</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Data Exchange Mode</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDataExchangeMode(DataExchangeMode object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>String Exchange Mode</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>String Exchange Mode</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseStringExchangeMode(StringExchangeMode object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>File Exchange Mode</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>File Exchange Mode</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFileExchangeMode(FileExchangeMode object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Data Exchange Element</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Data Exchange Element</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDataExchangeElement(DataExchangeElement object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Data Exchange Attribute</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Data Exchange Attribute</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDataExchangeAttribute(DataExchangeAttribute object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Value List Entry</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Value List Entry</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseValueListEntry(ValueListEntry object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Mapping Object</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Mapping Object</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseExchangeMappingObject(ExchangeMappingObject object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Mapping Attribute</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Mapping Attribute</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseExchangeMappingAttribute(ExchangeMappingAttribute object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Filter Method Parameter</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Filter Method Parameter</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseFilterMethodParameter(FilterMethodParameter object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Association Controller</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Association Controller</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAssociationController(AssociationController object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Direct Exchange Mode</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Direct Exchange Mode</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDirectExchangeMode(DirectExchangeMode object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Java Type</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Java Type</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaType(JavaType object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Service Bean</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Service Bean</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceBean(ServiceBean object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Java Method</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Java Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseJavaMethod(JavaMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Service Method</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Service Method</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseServiceMethod(ServiceMethod object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Mapping Object</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Mapping Object</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseMappingObject(MappingObject object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Mapping Attribute</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Mapping Attribute</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseMappingAttribute(MappingAttribute object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Method Parameter</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Method Parameter</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseMethodParameter(MethodParameter object) {
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
