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

import java.util.Arrays;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Java Type</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#isMappable <em>Mappable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#isPrimitive <em>Primitive</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#getNamespace <em>Namespace</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class JavaTypeImpl extends EObjectImpl implements JavaType {
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
	 * The default value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected static final String COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getComment() <em>Comment</em>}' attribute
	 * @see #getComment()
	 * @generated
	 * @ordered
	 */
	protected String comment = COMMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #isMappable() <em>Mappable</em>}' attribute
	 * @see #isMappable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MAPPABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isMappable() <em>Mappable</em>}' attribute
	 * @see #isMappable()
	 * @generated
	 * @ordered
	 */
	protected boolean mappable = MAPPABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isPrimitive() <em>Primitive</em>}' attribute
	 * @see #isPrimitive()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PRIMITIVE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPrimitive() <em>Primitive</em>}' attribute
	 * @see #isPrimitive()
	 * @generated
	 * @ordered
	 */
	protected boolean primitive = PRIMITIVE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getNamespace() <em>Namespace</em>}' reference
	 * @see #getNamespace()
	 * @generated
	 * @ordered
	 */
	protected Namespace namespace;

	/**
	 * @generated
	 */
	protected JavaTypeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return JavaPackage.Literals.JAVA_TYPE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getComment()
	 * @generated
	 */
	@Override
	public String getComment() {
		return comment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#setComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setComment(String newComment) {
		final String oldComment = comment;
		comment = newComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__COMMENT, oldComment, comment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isMappable()
	 * @generated
	 */
	@Override
	public boolean isMappable() {
		return mappable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#setMappable(boolean)
	 * @generated
	 */
	@Override
	public void setMappable(boolean newMappable) {
		final boolean oldMappable = mappable;
		mappable = newMappable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__MAPPABLE, oldMappable, mappable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isPrimitive()
	 * @generated
	 */
	@Override
	public boolean isPrimitive() {
		return primitive;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#setPrimitive(boolean)
	 * @generated
	 */
	@Override
	public void setPrimitive(boolean newPrimitive) {
		final boolean oldPrimitive = primitive;
		primitive = newPrimitive;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__PRIMITIVE, oldPrimitive, primitive));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getNamespace()
	 * @generated
	 */
	@Override
	public Namespace getNamespace() {
		if (namespace != null && namespace.eIsProxy()) {
			final var oldNamespace = (InternalEObject) namespace;
			namespace = (Namespace) eResolveProxy(oldNamespace);

			if (namespace != oldNamespace && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, JavaPackage.JAVA_TYPE__NAMESPACE, oldNamespace, namespace));
		}

		return namespace;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public Namespace basicGetNamespace() {
		return namespace;
	}

	/**
	 * @param newNamespace
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetNamespace(Namespace newNamespace, NotificationChain msgs) {
		final Namespace oldNamespace = namespace;
		namespace = newNamespace;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__NAMESPACE, oldNamespace,
					newNamespace);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#setNamespace(net.codecadenza.eclipse.model.java.Namespace)
	 * @generated
	 */
	@Override
	public void setNamespace(Namespace newNamespace) {
		if (newNamespace != namespace) {
			NotificationChain msgs = null;

			if (namespace != null)
				msgs = ((InternalEObject) namespace).eInverseRemove(this, JavaPackage.NAMESPACE__JAVA_TYPES, Namespace.class, msgs);

			if (newNamespace != null)
				msgs = ((InternalEObject) newNamespace).eInverseAdd(this, JavaPackage.NAMESPACE__JAVA_TYPES, Namespace.class, msgs);

			msgs = basicSetNamespace(newNamespace, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, JavaPackage.JAVA_TYPE__NAMESPACE, newNamespace, newNamespace));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				if (namespace != null)
					msgs = ((InternalEObject) namespace).eInverseRemove(this, JavaPackage.NAMESPACE__JAVA_TYPES, Namespace.class, msgs);

				return basicSetNamespace((Namespace) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				return basicSetNamespace(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case JavaPackage.JAVA_TYPE__NAME:
				return getName();
			case JavaPackage.JAVA_TYPE__COMMENT:
				return getComment();
			case JavaPackage.JAVA_TYPE__MAPPABLE:
				return isMappable();
			case JavaPackage.JAVA_TYPE__PRIMITIVE:
				return isPrimitive();
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				if (resolve)
					return getNamespace();

				return basicGetNamespace();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case JavaPackage.JAVA_TYPE__NAME:
				setName((String) newValue);
				return;
			case JavaPackage.JAVA_TYPE__COMMENT:
				setComment((String) newValue);
				return;
			case JavaPackage.JAVA_TYPE__MAPPABLE:
				setMappable((Boolean) newValue);
				return;
			case JavaPackage.JAVA_TYPE__PRIMITIVE:
				setPrimitive((Boolean) newValue);
				return;
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				setNamespace((Namespace) newValue);
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
			case JavaPackage.JAVA_TYPE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case JavaPackage.JAVA_TYPE__COMMENT:
				setComment(COMMENT_EDEFAULT);
				return;
			case JavaPackage.JAVA_TYPE__MAPPABLE:
				setMappable(MAPPABLE_EDEFAULT);
				return;
			case JavaPackage.JAVA_TYPE__PRIMITIVE:
				setPrimitive(PRIMITIVE_EDEFAULT);
				return;
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				setNamespace((Namespace) null);
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
			case JavaPackage.JAVA_TYPE__NAME:
				return name != null;
			case JavaPackage.JAVA_TYPE__COMMENT:
				return comment != null;
			case JavaPackage.JAVA_TYPE__MAPPABLE:
				return mappable != MAPPABLE_EDEFAULT;
			case JavaPackage.JAVA_TYPE__PRIMITIVE:
				return primitive != PRIMITIVE_EDEFAULT;
			case JavaPackage.JAVA_TYPE__NAMESPACE:
				return namespace != null;
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
		result.append(", comment: ");
		result.append(comment);
		result.append(", mappable: ");
		result.append(mappable);
		result.append(", primitive: ");
		result.append(primitive);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getUpperCaseName()
	 * @generated not
	 */
	@Override
	public String getUpperCaseName() {
		if (name == null || name.isEmpty())
			return "";

		if (name.length() == 1)
			return name.toUpperCase();

		return name.substring(0, 1).toUpperCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getLowerCaseName()
	 * @generated not
	 */
	@Override
	public String getLowerCaseName() {
		if (name == null || name.isEmpty())
			return "";

		if (name.length() == 1)
			return name.toLowerCase();

		return name.substring(0, 1).toLowerCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getDBColumnTypes(net.codecadenza.eclipse.model.project.Project)
	 * @generated not
	 */
	@Override
	public EList<DBColumnType> getDBColumnTypes(Project project) {
		final var result = new BasicEList<DBColumnType>();

		for (final DBColumnType t : project.getDatabase().getAllSupportedColumnTypes())
			for (final JavaType j : t.getJavaTypes())
				if (j.equals(this))
					result.add(t);

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isType(java.lang.String[])
	 * @generated not
	 */
	@Override
	public boolean isType(String... typeNames) {
		return Arrays.asList(typeNames).stream().anyMatch(typeName -> typeName.equals(name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isDecimalNumber()
	 * @generated not
	 */
	@Override
	public boolean isDecimalNumber() {
		return isFloat() || isDouble() || isBigDecimal();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isIntegerOrLong()
	 * @generated not
	 */
	@Override
	public boolean isIntegerOrLong() {
		return isInteger() || isLong();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isNumber()
	 * @generated not
	 */
	@Override
	public boolean isNumber() {
		return isIntegerOrLong() || isDecimalNumber();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isString()
	 * @generated not
	 */
	@Override
	public boolean isString() {
		return isType(STRING);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isInteger()
	 * @generated not
	 */
	@Override
	public boolean isInteger() {
		return isType(INT, INTEGER);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isLong()
	 * @generated not
	 */
	@Override
	public boolean isLong() {
		return isType(LONG, LONG_OBJ);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isFloat()
	 * @generated not
	 */
	@Override
	public boolean isFloat() {
		return isType(FLOAT, FLOAT_OBJ);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isDouble()
	 * @generated not
	 */
	@Override
	public boolean isDouble() {
		return isType(DOUBLE, DOUBLE_OBJ);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isBigDecimal()
	 * @generated not
	 */
	@Override
	public boolean isBigDecimal() {
		return isType(BIG_DECIMAL);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isBoolean()
	 * @generated not
	 */
	@Override
	public boolean isBoolean() {
		return isType(BOOL, BOOL_OBJ);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isChar()
	 * @generated not
	 */
	@Override
	public boolean isChar() {
		return isType(CHAR);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isByteArray()
	 * @generated not
	 */
	@Override
	public boolean isByteArray() {
		return isType(BYTE_ARRAY, BYTE_OBJ_ARRAY);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isDateOrCalendar()
	 * @generated not
	 */
	@Override
	public boolean isDateOrCalendar() {
		return isType(DATE, GREGORIAN_CAL);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isDate()
	 * @generated not
	 */
	@Override
	public boolean isDate() {
		return isType(DATE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isLocalDate()
	 * @generated not
	 */
	@Override
	public boolean isLocalDate() {
		return isType(LOCAL_DATE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isLocalDateTime()
	 * @generated not
	 */
	@Override
	public boolean isLocalDateTime() {
		return isType(LOCAL_DATE_TIME);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isCalendar()
	 * @generated not
	 */
	@Override
	public boolean isCalendar() {
		return isType(GREGORIAN_CAL);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isTemporalType()
	 * @generated not
	 */
	@Override
	public boolean isTemporalType() {
		return isDateOrCalendar() || isLocalDate() || isLocalDateTime();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isUUID()
	 * @generated not
	 */
	@Override
	public boolean isUUID() {
		return isType(UUID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isVoid()
	 * @generated not
	 */
	@Override
	public boolean isVoid() {
		return isType(VOID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#isEnum()
	 * @generated not
	 */
	@Override
	public boolean isEnum() {
		return this instanceof JavaEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getWrapperTypeName()
	 * @generated not
	 */
	@Override
	public String getWrapperTypeName() {
		if (isType(INT))
			return INTEGER;
		else if (isType(LONG))
			return LONG_OBJ;
		else if (isType(CHAR))
			return CHARACTER;
		else if (isType(DOUBLE))
			return DOUBLE_OBJ;
		else if (isType(FLOAT))
			return FLOAT_OBJ;
		else if (isType(VOID))
			return VOID_OBJ;
		else if (isType(BOOL))
			return BOOL_OBJ;

		return getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaType#getLocalVariableDefaultValue()
	 * @generated not
	 */
	@Override
	public String getLocalVariableDefaultValue() {
		if (isPrimitive()) {
			if (isType(INT, LONG, FLOAT, DOUBLE))
				return "0";
			else if (isType(BOOL))
				return "false";
			else if (isType(CHAR))
				return "'\\u0000'";
		}

		return "null";
	}

}
