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
package net.codecadenza.eclipse.model.java.impl;

import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.EnumTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
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
public class JavaFactoryImpl extends EFactoryImpl implements JavaFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static JavaFactory init() {
		try {
			final var theJavaFactory = (JavaFactory) EPackage.Registry.INSTANCE.getEFactory(JavaPackage.eNS_URI);

			if (theJavaFactory != null)
				return theJavaFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new JavaFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case JavaPackage.ENUM_LITERAL -> createEnumLiteral();
			case JavaPackage.JAVA_ENUM -> createJavaEnum();
			case JavaPackage.JAVA_METHOD -> createJavaMethod();
			case JavaPackage.JAVA_TYPE -> createJavaType();
			case JavaPackage.METHOD_PARAMETER -> createMethodParameter();
			case JavaPackage.NAMESPACE -> createNamespace();
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
			case JavaPackage.JAVA_TYPE_MODIFIER_ENUMERATION -> createJavaTypeModifierEnumerationFromString(eDataType, initialValue);
			case JavaPackage.ENUM_LITERAL_TAG_ENUMERATION -> createEnumLiteralTagEnumerationFromString(eDataType, initialValue);
			case JavaPackage.ENUM_TAG_ENUMERATION -> createEnumTagEnumerationFromString(eDataType, initialValue);
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
			case JavaPackage.JAVA_TYPE_MODIFIER_ENUMERATION -> convertJavaTypeModifierEnumerationToString(eDataType, instanceValue);
			case JavaPackage.ENUM_LITERAL_TAG_ENUMERATION -> convertEnumLiteralTagEnumerationToString(eDataType, instanceValue);
			case JavaPackage.ENUM_TAG_ENUMERATION -> convertEnumTagEnumerationToString(eDataType, instanceValue);
			default -> throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createEnumLiteral()
	 * @generated
	 */
	@Override
	public EnumLiteral createEnumLiteral() {
		return new EnumLiteralImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createJavaEnum()
	 * @generated
	 */
	@Override
	public JavaEnum createJavaEnum() {
		return new JavaEnumImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createJavaMethod()
	 * @generated
	 */
	@Override
	public JavaMethod createJavaMethod() {
		return new JavaMethodImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createJavaType()
	 * @generated
	 */
	@Override
	public JavaType createJavaType() {
		return new JavaTypeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createMethodParameter()
	 * @generated
	 */
	@Override
	public MethodParameter createMethodParameter() {
		return new MethodParameterImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#createNamespace()
	 * @generated
	 */
	@Override
	public Namespace createNamespace() {
		return new NamespaceImpl();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public JavaTypeModifierEnumeration createJavaTypeModifierEnumerationFromString(EDataType eDataType, String initialValue) {
		final JavaTypeModifierEnumeration result = JavaTypeModifierEnumeration.get(initialValue);

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
	public String convertJavaTypeModifierEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public EnumLiteralTagEnumeration createEnumLiteralTagEnumerationFromString(EDataType eDataType, String initialValue) {
		final EnumLiteralTagEnumeration result = EnumLiteralTagEnumeration.get(initialValue);

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
	public String convertEnumLiteralTagEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * @param eDataType
	 * @param initialValue
	 * @return an enumeration based on given parameters
	 * @generated
	 */
	public EnumTagEnumeration createEnumTagEnumerationFromString(EDataType eDataType, String initialValue) {
		final EnumTagEnumeration result = EnumTagEnumeration.get(initialValue);

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
	public String convertEnumTagEnumerationToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaFactory#getJavaPackage()
	 * @generated
	 */
	@Override
	public JavaPackage getJavaPackage() {
		return (JavaPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the Java package
	 * @generated
	 */
	@Deprecated
	public static JavaPackage getPackage() {
		return JavaPackage.eINSTANCE;
	}

}
