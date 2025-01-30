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
package net.codecadenza.eclipse.model.repository.impl;

import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryFactory;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
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
public class RepositoryFactoryImpl extends EFactoryImpl implements RepositoryFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static RepositoryFactory init() {
		try {
			final var theRepositoryFactory = (RepositoryFactory) EPackage.Registry.INSTANCE.getEFactory(RepositoryPackage.eNS_URI);

			if (theRepositoryFactory != null)
				return theRepositoryFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new RepositoryFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case RepositoryPackage.REPOSITORY -> createRepository();
			case RepositoryPackage.REPOSITORY_METHOD -> createRepositoryMethod();
			case RepositoryPackage.REPOSITORY_METHOD_PARAMETER -> createRepositoryMethodParameter();
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
			case RepositoryPackage.REPOSITORY_METHOD_TYPE_ENUMERATION -> createRepositoryMethodTypeEnumerationFromString(eDataType,
					initialValue);
			case RepositoryPackage.PERMISSION_MODE_ENUMERATION -> createPermissionModeEnumerationFromString(eDataType, initialValue);
			case RepositoryPackage.TRANSACTION_TYPE_ENUMERATION -> createTransactionTypeEnumerationFromString(eDataType, initialValue);
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
			case RepositoryPackage.REPOSITORY_METHOD_TYPE_ENUMERATION -> convertRepositoryMethodTypeEnumerationToString(eDataType,
					instanceValue);
			case RepositoryPackage.PERMISSION_MODE_ENUMERATION -> convertPermissionModeEnumerationToString(eDataType, instanceValue);
			case RepositoryPackage.TRANSACTION_TYPE_ENUMERATION -> convertTransactionTypeEnumerationToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryFactory#createRepository()
	 * @generated
	 */
	@Override
	public Repository createRepository() {
		return new RepositoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryFactory#createRepositoryMethod()
	 * @generated
	 */
	@Override
	public RepositoryMethod createRepositoryMethod() {
		return new RepositoryMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryFactory#createRepositoryMethodParameter()
	 * @generated
	 */
	@Override
	public RepositoryMethodParameter createRepositoryMethodParameter() {
		return new RepositoryMethodParameterImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public RepositoryMethodTypeEnumeration createRepositoryMethodTypeEnumerationFromString(EDataType eDataType,
			String initialValue) {
		final RepositoryMethodTypeEnumeration result = RepositoryMethodTypeEnumeration.get(initialValue);

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
	public String convertRepositoryMethodTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public PermissionModeEnumeration createPermissionModeEnumerationFromString(EDataType eDataType, String initialValue) {
		final PermissionModeEnumeration result = PermissionModeEnumeration.get(initialValue);

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
	public String convertPermissionModeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public TransactionTypeEnumeration createTransactionTypeEnumerationFromString(EDataType eDataType, String initialValue) {
		final TransactionTypeEnumeration result = TransactionTypeEnumeration.get(initialValue);

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
	public String convertTransactionTypeEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryFactory#getRepositoryPackage()
	 * @generated
	 */
	@Override
	public RepositoryPackage getRepositoryPackage() {
		return (RepositoryPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the repository package
	 * @generated
	 */
	@Deprecated
	public static RepositoryPackage getPackage() {
		return RepositoryPackage.eINSTANCE;
	}

}
