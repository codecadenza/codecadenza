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
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage
 * @generated
 */
public class DomainAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static DomainPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public DomainAdapterFactory() {
		if (modelPackage == null)
			modelPackage = DomainPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#isFactoryForType(java.lang.Object)
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected DomainSwitch<Adapter> modelSwitch = new DomainSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseAbstractDomainAssociation(net.codecadenza.eclipse.model.
		 * domain.AbstractDomainAssociation)
		 */
		@Override
		public Adapter caseAbstractDomainAssociation(AbstractDomainAssociation object) {
			return createAbstractDomainAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseDomainObject(net.codecadenza.eclipse.model.domain.
		 * DomainObject)
		 */
		@Override
		public Adapter caseDomainObject(DomainObject object) {
			return createDomainObjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseDomainAttribute(net.codecadenza.eclipse.model.domain.
		 * DomainAttribute)
		 */
		@Override
		public Adapter caseDomainAttribute(DomainAttribute object) {
			return createDomainAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseDomainAttributeValidator(net.codecadenza.eclipse.model.
		 * domain.DomainAttributeValidator)
		 */
		@Override
		public Adapter caseDomainAttributeValidator(DomainAttributeValidator object) {
			return createDomainAttributeValidatorAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseDomainInheritance(net.codecadenza.eclipse.model.domain.
		 * DomainInheritance)
		 */
		@Override
		public Adapter caseDomainInheritance(DomainInheritance object) {
			return createDomainInheritanceAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseDomainNamespace(net.codecadenza.eclipse.model.domain.
		 * DomainNamespace)
		 */
		@Override
		public Adapter caseDomainNamespace(DomainNamespace object) {
			return createDomainNamespaceAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseEnumAssociation(net.codecadenza.eclipse.model.domain.
		 * EnumAssociation)
		 */
		@Override
		public Adapter caseEnumAssociation(EnumAssociation object) {
			return createEnumAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseIDGenerator(net.codecadenza.eclipse.model.domain.IDGenerator)
		 */
		@Override
		public Adapter caseIDGenerator(IDGenerator object) {
			return createIDGeneratorAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseManyToManyAssociation(net.codecadenza.eclipse.model.domain.
		 * ManyToManyAssociation)
		 */
		@Override
		public Adapter caseManyToManyAssociation(ManyToManyAssociation object) {
			return createManyToManyAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseManyToOneAssociation(net.codecadenza.eclipse.model.domain.
		 * ManyToOneAssociation)
		 */
		@Override
		public Adapter caseManyToOneAssociation(ManyToOneAssociation object) {
			return createManyToOneAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseOneToManyAssociation(net.codecadenza.eclipse.model.domain.
		 * OneToManyAssociation)
		 */
		@Override
		public Adapter caseOneToManyAssociation(OneToManyAssociation object) {
			return createOneToManyAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseOneToOneAssociation(net.codecadenza.eclipse.model.domain.
		 * OneToOneAssociation)
		 */
		@Override
		public Adapter caseOneToOneAssociation(OneToOneAssociation object) {
			return createOneToOneAssociationAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseJavaType(net.codecadenza.eclipse.model.java.JavaType)
		 */
		@Override
		public Adapter caseJavaType(JavaType object) {
			return createJavaTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#caseNamespace(net.codecadenza.eclipse.model.java.Namespace)
		 */
		@Override
		public Adapter caseNamespace(Namespace object) {
			return createNamespaceAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.domain.util.DomainSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation
	 * <em>Abstract Domain Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's
	 * useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation
	 * @generated
	 */
	public Adapter createAbstractDomainAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.DomainObject <em>Object</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.DomainObject
	 * @generated
	 */
	public Adapter createDomainObjectAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.DomainAttribute <em>Domain
	 * Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute
	 * @generated
	 */
	public Adapter createDomainAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator
	 * <em>Attribute Validator</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to
	 * ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator
	 * @generated
	 */
	public Adapter createDomainAttributeValidatorAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.DomainInheritance
	 * <em>Inheritance</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance
	 * @generated
	 */
	public Adapter createDomainInheritanceAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.DomainNamespace <em>Namespace</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace
	 * @generated
	 */
	public Adapter createDomainNamespaceAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.EnumAssociation <em>Enum
	 * Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation
	 * @generated
	 */
	public Adapter createEnumAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.IDGenerator <em>ID Generator</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator
	 * @generated
	 */
	public Adapter createIDGeneratorAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.ManyToManyAssociation <em>Many To
	 * Many Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.ManyToManyAssociation
	 * @generated
	 */
	public Adapter createManyToManyAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation <em>Many To One
	 * Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation
	 * @generated
	 */
	public Adapter createManyToOneAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.OneToManyAssociation <em>One To Many
	 * Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation
	 * @generated
	 */
	public Adapter createOneToManyAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation <em>One To One
	 * Association</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation
	 * @generated
	 */
	public Adapter createOneToOneAssociationAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaType <em>Java Type</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaType
	 * @generated
	 */
	public Adapter createJavaTypeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.Namespace <em>Namespace</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.Namespace
	 * @generated
	 */
	public Adapter createNamespaceAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
