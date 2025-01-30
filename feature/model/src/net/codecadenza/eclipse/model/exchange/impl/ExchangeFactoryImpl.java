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
package net.codecadenza.eclipse.model.exchange.impl;

import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class ExchangeFactoryImpl extends EFactoryImpl implements ExchangeFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static ExchangeFactory init() {
		try {
			final var theExchangeFactory = (ExchangeFactory) EPackage.Registry.INSTANCE.getEFactory(ExchangePackage.eNS_URI);

			if (theExchangeFactory != null)
				return theExchangeFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new ExchangeFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN -> createDataExchangeServiceBean();
			case ExchangePackage.DATA_EXCHANGE_METHOD -> createDataExchangeMethod();
			case ExchangePackage.STRING_EXCHANGE_MODE -> createStringExchangeMode();
			case ExchangePackage.FILE_EXCHANGE_MODE -> createFileExchangeMode();
			case ExchangePackage.DIRECT_EXCHANGE_MODE -> createDirectExchangeMode();
			case ExchangePackage.DATA_EXCHANGE_ELEMENT -> createDataExchangeElement();
			case ExchangePackage.DATA_EXCHANGE_ATTRIBUTE -> createDataExchangeAttribute();
			case ExchangePackage.VALUE_LIST_ENTRY -> createValueListEntry();
			case ExchangePackage.EXCHANGE_MAPPING_OBJECT -> createExchangeMappingObject();
			case ExchangePackage.EXCHANGE_MAPPING_ATTRIBUTE -> createExchangeMappingAttribute();
			case ExchangePackage.FILTER_METHOD_PARAMETER -> createFilterMethodParameter();
			case ExchangePackage.ASSOCIATION_CONTROLLER -> createAssociationController();
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
			case ExchangePackage.DATA_EXCHANGE_METHOD_TYPE_ENUMERATION -> createDataExchangeMethodTypeEnumerationFromString(eDataType,
					initialValue);
			case ExchangePackage.CONTENT_TYPE_ENUMERATION -> createContentTypeEnumerationFromString(eDataType, initialValue);
			case ExchangePackage.PARSER_IMPLEMENTATION_ENUMERATION -> createParserImplementationEnumerationFromString(eDataType,
					initialValue);
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
			case ExchangePackage.DATA_EXCHANGE_METHOD_TYPE_ENUMERATION -> convertDataExchangeMethodTypeEnumerationToString(eDataType,
					instanceValue);
			case ExchangePackage.CONTENT_TYPE_ENUMERATION -> convertContentTypeEnumerationToString(eDataType, instanceValue);
			case ExchangePackage.PARSER_IMPLEMENTATION_ENUMERATION -> convertParserImplementationEnumerationToString(eDataType,
					instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createDataExchangeServiceBean()
	 * @generated
	 */
	@Override
	public DataExchangeServiceBean createDataExchangeServiceBean() {
		return new DataExchangeServiceBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createDataExchangeMethod()
	 * @generated
	 */
	@Override
	public DataExchangeMethod createDataExchangeMethod() {
		return new DataExchangeMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createStringExchangeMode()
	 * @generated
	 */
	@Override
	public StringExchangeMode createStringExchangeMode() {
		return new StringExchangeModeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createFileExchangeMode()
	 * @generated
	 */
	@Override
	public FileExchangeMode createFileExchangeMode() {
		return new FileExchangeModeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createDataExchangeElement()
	 * @generated
	 */
	@Override
	public DataExchangeElement createDataExchangeElement() {
		return new DataExchangeElementImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createDataExchangeAttribute()
	 * @generated
	 */
	@Override
	public DataExchangeAttribute createDataExchangeAttribute() {
		return new DataExchangeAttributeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createValueListEntry()
	 * @generated
	 */
	@Override
	public ValueListEntry createValueListEntry() {
		return new ValueListEntryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createExchangeMappingObject()
	 * @generated
	 */
	@Override
	public ExchangeMappingObject createExchangeMappingObject() {
		return new ExchangeMappingObjectImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createExchangeMappingAttribute()
	 * @generated
	 */
	@Override
	public ExchangeMappingAttribute createExchangeMappingAttribute() {
		return new ExchangeMappingAttributeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createFilterMethodParameter()
	 * @generated
	 */
	@Override
	public FilterMethodParameter createFilterMethodParameter() {
		return new FilterMethodParameterImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createAssociationController()
	 * @generated
	 */
	@Override
	public AssociationController createAssociationController() {
		return new AssociationControllerImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#createDirectExchangeMode()
	 * @generated
	 */
	@Override
	public DirectExchangeMode createDirectExchangeMode() {
		return new DirectExchangeModeImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public DataExchangeMethodTypeEnumeration createDataExchangeMethodTypeEnumerationFromString(EDataType eDataType,
			String initialValue) {
		final DataExchangeMethodTypeEnumeration result = DataExchangeMethodTypeEnumeration.get(initialValue);

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
	public String convertDataExchangeMethodTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ContentTypeEnumeration createContentTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final ContentTypeEnumeration result = ContentTypeEnumeration.get(initialValue);

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
	public String convertContentTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public ParserImplementationEnumeration createParserImplementationEnumerationFromString(EDataType eDataType,
			String initialValue) {
		final ParserImplementationEnumeration result = ParserImplementationEnumeration.get(initialValue);

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
	public String convertParserImplementationEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory#getExchangePackage()
	 * @generated
	 */
	@Override
	public ExchangePackage getExchangePackage() {
		return (ExchangePackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the exchange package
	 * @generated
	 */
	@Deprecated
	public static ExchangePackage getPackage() {
		return ExchangePackage.eINSTANCE;
	}

}
