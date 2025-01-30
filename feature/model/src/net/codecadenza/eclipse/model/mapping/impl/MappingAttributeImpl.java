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
package net.codecadenza.eclipse.model.mapping.impl;

import java.util.Collection;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * An implementation of the model object '<em><b>Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getDomainAttribute <em>Domain Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getAssociationList <em>Association List</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getMappingType <em>Mapping Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getModifier <em>Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getDefaultValue <em>Default Value</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public abstract class MappingAttributeImpl extends EObjectImpl implements MappingAttribute {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getAssociation() <em>Association</em>}' reference
	 * @see #getAssociation()
	 * @generated
	 * @ordered
	 */
	protected AbstractDomainAssociation association;

	/**
	 * The cached value of the '{@link #getDomainAttribute() <em>Domain Attribute</em>}' reference
	 * @see #getDomainAttribute()
	 * @generated
	 * @ordered
	 */
	protected DomainAttribute domainAttribute;

	/**
	 * The cached value of the '{@link #getAssociationList() <em>Association List</em>}' reference list
	 * @see #getAssociationList()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractDomainAssociation> associationList;

	/**
	 * The cached value of the '{@link #getMappingType() <em>Mapping Type</em>}' reference
	 * @see #getMappingType()
	 * @generated
	 * @ordered
	 */
	protected JavaType mappingType;

	/**
	 * The default value of the '{@link #getModifier() <em>Modifier</em>}' attribute
	 * @see #getModifier()
	 * @generated
	 * @ordered
	 */
	protected static final JavaTypeModifierEnumeration MODIFIER_EDEFAULT = JavaTypeModifierEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getModifier() <em>Modifier</em>}' attribute
	 * @see #getModifier()
	 * @generated
	 * @ordered
	 */
	protected JavaTypeModifierEnumeration modifier = MODIFIER_EDEFAULT;

	/**
	 * The default value of the '{@link #getDefaultValue() <em>Default Value</em>}' attribute
	 * @see #getDefaultValue()
	 * @generated
	 * @ordered
	 */
	protected static final String DEFAULT_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultValue() <em>Default Value</em>}' attribute
	 * @see #getDefaultValue()
	 * @generated
	 * @ordered
	 */
	protected String defaultValue = DEFAULT_VALUE_EDEFAULT;

	/**
	 * @generated
	 */
	protected MappingAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return MappingPackage.Literals.MAPPING_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociation()
	 * @generated
	 */
	@Override
	public AbstractDomainAssociation getAssociation() {
		if (association != null && association.eIsProxy()) {
			final var oldAssociation = (InternalEObject) association;
			association = (AbstractDomainAssociation) eResolveProxy(oldAssociation);

			if (association != oldAssociation && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION, oldAssociation,
						association));
		}

		return association;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public AbstractDomainAssociation basicGetAssociation() {
		return association;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setAssociation(net.codecadenza.eclipse.model.domain.
	 * AbstractDomainAssociation)
	 * @generated
	 */
	@Override
	public void setAssociation(AbstractDomainAssociation newAssociation) {
		final AbstractDomainAssociation oldAssociation = association;
		association = newAssociation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION, oldAssociation,
					association));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getDomainAttribute()
	 * @generated
	 */
	@Override
	public DomainAttribute getDomainAttribute() {
		if (domainAttribute != null && domainAttribute.eIsProxy()) {
			final var oldDomainAttribute = (InternalEObject) domainAttribute;
			domainAttribute = (DomainAttribute) eResolveProxy(oldDomainAttribute);

			if (domainAttribute != oldDomainAttribute && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE,
						oldDomainAttribute, domainAttribute));
		}

		return domainAttribute;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainAttribute basicGetDomainAttribute() {
		return domainAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setDomainAttribute(net.codecadenza.eclipse.model.domain.
	 * DomainAttribute)
	 * @generated
	 */
	@Override
	public void setDomainAttribute(DomainAttribute newDomainAttribute) {
		final DomainAttribute oldDomainAttribute = domainAttribute;
		domainAttribute = newDomainAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE,
					oldDomainAttribute, domainAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationList()
	 * @generated
	 */
	@Override
	public EList<AbstractDomainAssociation> getAssociationList() {
		if (associationList == null)
			associationList = new EObjectResolvingEList<>(AbstractDomainAssociation.class, this,
					MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST);

		return associationList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getMappingType()
	 * @generated
	 */
	@Override
	public JavaType getMappingType() {
		if (mappingType != null && mappingType.eIsProxy()) {
			final var oldMappingType = (InternalEObject) mappingType;
			mappingType = (JavaType) eResolveProxy(oldMappingType);

			if (mappingType != oldMappingType && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE, oldMappingType,
						mappingType));
		}

		return mappingType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetMappingType() {
		return mappingType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setMappingType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setMappingType(JavaType newMappingType) {
		final JavaType oldMappingType = mappingType;
		mappingType = newMappingType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE, oldMappingType,
					mappingType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getModifier()
	 * @generated
	 */
	@Override
	public JavaTypeModifierEnumeration getModifier() {
		return modifier;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setModifier(net.codecadenza.eclipse.model.java.
	 * JavaTypeModifierEnumeration)
	 * @generated
	 */
	@Override
	public void setModifier(JavaTypeModifierEnumeration newModifier) {
		final JavaTypeModifierEnumeration oldModifier = modifier;
		modifier = newModifier == null ? MODIFIER_EDEFAULT : newModifier;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__MODIFIER, oldModifier, modifier));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getDefaultValue()
	 * @generated
	 */
	@Override
	public String getDefaultValue() {
		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#setDefaultValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDefaultValue(String newDefaultValue) {
		final String oldDefaultValue = defaultValue;
		defaultValue = newDefaultValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE, oldDefaultValue,
					defaultValue));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case MappingPackage.MAPPING_ATTRIBUTE__NAME:
				return getName();
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION:
				if (resolve)
					return getAssociation();

				return basicGetAssociation();
			case MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE:
				if (resolve)
					return getDomainAttribute();

				return basicGetDomainAttribute();
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST:
				return getAssociationList();
			case MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE:
				if (resolve)
					return getMappingType();

				return basicGetMappingType();
			case MappingPackage.MAPPING_ATTRIBUTE__MODIFIER:
				return getModifier();
			case MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE:
				return getDefaultValue();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case MappingPackage.MAPPING_ATTRIBUTE__NAME:
				setName((String) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST:
				getAssociationList().clear();
				getAssociationList().addAll((Collection<? extends AbstractDomainAssociation>) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE:
				setMappingType((JavaType) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__MODIFIER:
				setModifier((JavaTypeModifierEnumeration) newValue);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE:
				setDefaultValue((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case MappingPackage.MAPPING_ATTRIBUTE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION:
				setAssociation((AbstractDomainAssociation) null);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE:
				setDomainAttribute((DomainAttribute) null);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST:
				getAssociationList().clear();
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE:
				setMappingType((JavaType) null);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__MODIFIER:
				setModifier(MODIFIER_EDEFAULT);
				return;
			case MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE:
				setDefaultValue(DEFAULT_VALUE_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case MappingPackage.MAPPING_ATTRIBUTE__NAME:
				return name != null;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION:
				return association != null;
			case MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE:
				return domainAttribute != null;
			case MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST:
				return associationList != null && !associationList.isEmpty();
			case MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE:
				return mappingType != null;
			case MappingPackage.MAPPING_ATTRIBUTE__MODIFIER:
				return modifier != MODIFIER_EDEFAULT;
			case MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE:
				return defaultValue != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", modifier: ");
		result.append(modifier);
		result.append(", defaultValue: ");
		result.append(defaultValue);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getUpperCaseName()
	 * @generated not
	 */
	@Override
	public String getUpperCaseName() {
		if (name.isEmpty())
			return "";

		if (name.length() == 1)
			return name.toUpperCase();

		return name.substring(0, 1).toUpperCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getGetterName()
	 * @generated not
	 */
	@Override
	public String getGetterName() {
		final boolean isBoolean = getDomainAttribute() != null && getSearchType().isType(JavaType.BOOL);

		return JavaBeanHelper.getGetterName(name, isBoolean);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getSetterName()
	 * @generated not
	 */
	@Override
	public String getSetterName() {
		return JavaBeanHelper.getSetterName(name);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getGetterReference()
	 * @generated not
	 */
	@Override
	public String getGetterReference() {
		final boolean isBoolean = getDomainAttribute() != null && getSearchType().isType(JavaType.BOOL);

		return JavaBeanHelper.getGetterReference(name, isBoolean);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getSetterReference()
	 * @generated not
	 */
	@Override
	public String getSetterReference() {
		return "::" + getSetterName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationListGetter()
	 * @generated not
	 */
	@Override
	public String getAssociationListGetter() {
		final var getters = new StringBuilder();

		if (getAssociationList() == null || getAssociationList().isEmpty())
			return getters.toString();

		if (!getAssociationList().isEmpty()) {
			for (int i = getAssociationList().size() - 1; i >= 0; i--) {
				final AbstractDomainAssociation ia = getAssociationList().get(i);

				getters.append(ia.getGetterName() + ".");
			}
		}

		return getters.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationListNullCheck(java.lang.String, boolean)
	 * @generated not
	 */
	@Override
	public String getAssociationListNullCheck(String objectName, boolean closeStatement) {
		final var nullCheck = new StringBuilder();
		final var getter = new StringBuilder();
		var first = true;

		if (getAssociation() == null)
			return nullCheck.toString();

		for (int i = getAssociationList().size() - 1; i >= 0; i--) {
			final AbstractDomainAssociation assoc = getAssociationList().get(i);
			boolean iaOpt = false;

			if (assoc instanceof final ManyToOneAssociation mto)
				iaOpt = mto.isOptional();
			else if (assoc instanceof final OneToOneAssociation oto)
				iaOpt = oto.isOptional();

			getter.append(assoc.getGetterName());

			if (iaOpt) {
				if (first)
					nullCheck.append("if(");
				else
					nullCheck.append(" && ");

				nullCheck.append(objectName + "." + getter.toString() + " != null");
				first = false;
			}

			getter.append(".");
		}

		if (closeStatement && !nullCheck.isEmpty())
			nullCheck.append(")\n");

		return nullCheck.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getNullCheck(java.lang.String, java.lang.String)
	 * @generated not
	 */
	@Override
	public String getNullCheck(String objectName, String additionalCondition) {
		final var nullCheck = new StringBuilder();
		var isOptional = false;
		var first = true;

		if (getAssociation() != null) {
			nullCheck.append(getAssociationListNullCheck(objectName, false));

			if (!nullCheck.isEmpty())
				first = false;

			if (getAssociation() instanceof final OneToOneAssociation oto)
				isOptional = oto.isOptional();
			else if (getAssociation() instanceof final ManyToOneAssociation mto)
				isOptional = mto.isOptional();

			if (isOptional) {
				if (first)
					nullCheck.append("if(");
				else
					nullCheck.append(" && ");

				nullCheck.append(objectName + "." + getAssociationListGetter());
				nullCheck.append(getAssociation().getGetterName() + " != null");
				first = false;
			}
		}

		if (additionalCondition != null && !additionalCondition.isEmpty()) {
			if (first)
				nullCheck.append("if(");
			else
				nullCheck.append(" && ");

			nullCheck.append(additionalCondition);
		}

		if (!nullCheck.isEmpty())
			nullCheck.append(")\n");

		return nullCheck.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute#getSearchType()
	 * @generated not
	 */
	@Override
	public JavaType getSearchType() {
		if (getDomainAttribute() == null)
			throw new IllegalStateException("The attribute '" + getName() + "' is not mapped to a domain attribute!");

		return getDomainAttribute().getJavaType();
	}

}
