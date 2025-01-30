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
package net.codecadenza.eclipse.model.domain.util;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage
 * @generated
 */
public class DomainSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static DomainPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public DomainSwitch() {
		if (modelPackage == null)
			modelPackage = DomainPackage.eINSTANCE;
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
			case DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION: {
				final var abstractDomainAssociation = (AbstractDomainAssociation) theEObject;
				T result = caseAbstractDomainAssociation(abstractDomainAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.DOMAIN_OBJECT: {
				final var domainObject = (DomainObject) theEObject;
				T result = caseDomainObject(domainObject);

				if (result == null)
					result = caseJavaType(domainObject);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.DOMAIN_ATTRIBUTE: {
				final var domainAttribute = (DomainAttribute) theEObject;
				T result = caseDomainAttribute(domainAttribute);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR: {
				final var domainAttributeValidator = (DomainAttributeValidator) theEObject;
				T result = caseDomainAttributeValidator(domainAttributeValidator);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.DOMAIN_INHERITANCE: {
				final var domainInheritance = (DomainInheritance) theEObject;
				T result = caseDomainInheritance(domainInheritance);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.DOMAIN_NAMESPACE: {
				final var domainNamespace = (DomainNamespace) theEObject;
				T result = caseDomainNamespace(domainNamespace);

				if (result == null)
					result = caseNamespace(domainNamespace);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.ENUM_ASSOCIATION: {
				final var enumAssociation = (EnumAssociation) theEObject;
				T result = caseEnumAssociation(enumAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.ID_GENERATOR: {
				final var idGenerator = (IDGenerator) theEObject;
				T result = caseIDGenerator(idGenerator);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.MANY_TO_MANY_ASSOCIATION: {
				final var manyToManyAssociation = (ManyToManyAssociation) theEObject;
				T result = caseManyToManyAssociation(manyToManyAssociation);

				if (result == null)
					result = caseAbstractDomainAssociation(manyToManyAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.MANY_TO_ONE_ASSOCIATION: {
				final var manyToOneAssociation = (ManyToOneAssociation) theEObject;
				T result = caseManyToOneAssociation(manyToOneAssociation);

				if (result == null)
					result = caseAbstractDomainAssociation(manyToOneAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.ONE_TO_MANY_ASSOCIATION: {
				final var oneToManyAssociation = (OneToManyAssociation) theEObject;
				T result = caseOneToManyAssociation(oneToManyAssociation);

				if (result == null)
					result = caseAbstractDomainAssociation(oneToManyAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case DomainPackage.ONE_TO_ONE_ASSOCIATION: {
				final var oneToOneAssociation = (OneToOneAssociation) theEObject;
				T result = caseOneToOneAssociation(oneToOneAssociation);

				if (result == null)
					result = caseAbstractDomainAssociation(oneToOneAssociation);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Abstract Domain Association</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Abstract Domain Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseAbstractDomainAssociation(AbstractDomainAssociation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Object</em>'. This implementation returns null; returning
	 * a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Object</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDomainObject(DomainObject object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Domain Attribute</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Domain Attribute</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDomainAttribute(DomainAttribute object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Attribute Validator</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Attribute Validator</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDomainAttributeValidator(DomainAttributeValidator object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Inheritance</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Inheritance</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDomainInheritance(DomainInheritance object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Namespace</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Namespace</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDomainNamespace(DomainNamespace object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Enum Association</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Enum Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseEnumAssociation(EnumAssociation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>ID Generator</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>ID Generator</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseIDGenerator(IDGenerator object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Many To Many Association</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Many To Many Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseManyToManyAssociation(ManyToManyAssociation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Many To One Association</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Many To One Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseManyToOneAssociation(ManyToOneAssociation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>One To Many Association</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>One To Many Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseOneToManyAssociation(OneToManyAssociation object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>One To One Association</em>'. This implementation returns
	 * null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>One To One Association</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseOneToOneAssociation(OneToOneAssociation object) {
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
	 * Return the result of interpreting the object as an instance of '<em>Namespace</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Namespace</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseNamespace(Namespace object) {
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
