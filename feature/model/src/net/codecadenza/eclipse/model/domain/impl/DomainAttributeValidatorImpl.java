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
package net.codecadenza.eclipse.model.domain.impl;

import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

/**
 * An implementation of the model object '<em><b>Attribute Validator</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#isFutureDate <em>Future Date</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#isPastDate <em>Past Date</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#getMaxLength <em>Max Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#getMinLength <em>Min Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#isNullable <em>Nullable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#getMaxValue <em>Max Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#getMinValue <em>Min Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl#getRegularExpression <em>Regular
 * Expression</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DomainAttributeValidatorImpl extends EObjectImpl implements DomainAttributeValidator {
	/**
	 * The default value of the '{@link #isFutureDate() <em>Future Date</em>}' attribute
	 * @see #isFutureDate()
	 * @generated
	 * @ordered
	 */
	protected static final boolean FUTURE_DATE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isFutureDate() <em>Future Date</em>}' attribute
	 * @see #isFutureDate()
	 * @generated
	 * @ordered
	 */
	protected boolean futureDate = FUTURE_DATE_EDEFAULT;

	/**
	 * The default value of the '{@link #isPastDate() <em>Past Date</em>}' attribute
	 * @see #isPastDate()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PAST_DATE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPastDate() <em>Past Date</em>}' attribute
	 * @see #isPastDate()
	 * @generated
	 * @ordered
	 */
	protected boolean pastDate = PAST_DATE_EDEFAULT;

	/**
	 * The default value of the '{@link #getMaxLength() <em>Max Length</em>}' attribute
	 * @see #getMaxLength()
	 * @generated
	 * @ordered
	 */
	protected static final Integer MAX_LENGTH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMaxLength() <em>Max Length</em>}' attribute
	 * @see #getMaxLength()
	 * @generated
	 * @ordered
	 */
	protected Integer maxLength = MAX_LENGTH_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinLength() <em>Min Length</em>}' attribute
	 * @see #getMinLength()
	 * @generated
	 * @ordered
	 */
	protected static final Integer MIN_LENGTH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMinLength() <em>Min Length</em>}' attribute
	 * @see #getMinLength()
	 * @generated
	 * @ordered
	 */
	protected Integer minLength = MIN_LENGTH_EDEFAULT;

	/**
	 * The default value of the '{@link #isNullable() <em>Nullable</em>}' attribute
	 * @see #isNullable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean NULLABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isNullable() <em>Nullable</em>}' attribute
	 * @see #isNullable()
	 * @generated
	 * @ordered
	 */
	protected boolean nullable = NULLABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #getMaxValue() <em>Max Value</em>}' attribute
	 * @see #getMaxValue()
	 * @generated
	 * @ordered
	 */
	protected static final String MAX_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMaxValue() <em>Max Value</em>}' attribute
	 * @see #getMaxValue()
	 * @generated
	 * @ordered
	 */
	protected String maxValue = MAX_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinValue() <em>Min Value</em>}' attribute
	 * @see #getMinValue()
	 * @generated
	 * @ordered
	 */
	protected static final String MIN_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMinValue() <em>Min Value</em>}' attribute
	 * @see #getMinValue()
	 * @generated
	 * @ordered
	 */
	protected String minValue = MIN_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getRegularExpression() <em>Regular Expression</em>}' attribute
	 * @see #getRegularExpression()
	 * @generated
	 * @ordered
	 */
	protected static final String REGULAR_EXPRESSION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRegularExpression() <em>Regular Expression</em>}' attribute
	 * @see #getRegularExpression()
	 * @generated
	 * @ordered
	 */
	protected String regularExpression = REGULAR_EXPRESSION_EDEFAULT;

	/**
	 * @generated
	 */
	protected DomainAttributeValidatorImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.DOMAIN_ATTRIBUTE_VALIDATOR;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isFutureDate()
	 * @generated
	 */
	@Override
	public boolean isFutureDate() {
		return futureDate;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setFutureDate(boolean)
	 * @generated
	 */
	@Override
	public void setFutureDate(boolean newFutureDate) {
		final boolean oldFutureDate = futureDate;
		futureDate = newFutureDate;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE, oldFutureDate,
					futureDate));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isPastDate()
	 * @generated
	 */
	@Override
	public boolean isPastDate() {
		return pastDate;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setPastDate(boolean)
	 * @generated
	 */
	@Override
	public void setPastDate(boolean newPastDate) {
		final boolean oldPastDate = pastDate;
		pastDate = newPastDate;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE, oldPastDate,
					pastDate));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxLength()
	 * @generated
	 */
	@Override
	public Integer getMaxLength() {
		return maxLength;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setMaxLength(Integer)
	 * @generated
	 */
	@Override
	public void setMaxLength(Integer newMaxLength) {
		final Integer oldMaxLength = maxLength;
		maxLength = newMaxLength;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH, oldMaxLength,
					maxLength));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinLength()
	 * @generated
	 */
	@Override
	public Integer getMinLength() {
		return minLength;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setMinLength(int)
	 * @generated
	 */
	@Override
	public void setMinLength(Integer newMinLength) {
		final Integer oldMinLength = minLength;
		minLength = newMinLength;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH, oldMinLength,
					minLength));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isNullable()
	 * @generated
	 */
	@Override
	public boolean isNullable() {
		return nullable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setNullable(boolean)
	 * @generated
	 */
	@Override
	public void setNullable(boolean newNullable) {
		final boolean oldNullable = nullable;
		nullable = newNullable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE, oldNullable,
					nullable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxValue()
	 * @generated
	 */
	@Override
	public String getMaxValue() {
		return maxValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setMaxValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setMaxValue(String newMaxValue) {
		final String oldMaxValue = maxValue;
		maxValue = newMaxValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE, oldMaxValue,
					maxValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinValue()
	 * @generated
	 */
	@Override
	public String getMinValue() {
		return minValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setMinValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setMinValue(String newMinValue) {
		final String oldMinValue = minValue;
		minValue = newMinValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE, oldMinValue,
					minValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getRegularExpression()
	 * @generated
	 */
	@Override
	public String getRegularExpression() {
		return regularExpression;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#setRegularExpression(java.lang.String)
	 * @generated
	 */
	@Override
	public void setRegularExpression(String newRegularExpression) {
		final String oldRegularExpression = regularExpression;
		regularExpression = newRegularExpression;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION,
					oldRegularExpression, regularExpression));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE:
				return isFutureDate();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE:
				return isPastDate();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH:
				return getMaxLength();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH:
				return getMinLength();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE:
				return isNullable();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE:
				return getMaxValue();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE:
				return getMinValue();
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION:
				return getRegularExpression();
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
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE:
				setFutureDate((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE:
				setPastDate((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH:
				setMaxLength((Integer) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH:
				setMinLength((Integer) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE:
				setNullable((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE:
				setMaxValue((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE:
				setMinValue((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION:
				setRegularExpression((String) newValue);
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
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE:
				setFutureDate(FUTURE_DATE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE:
				setPastDate(PAST_DATE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH:
				setMaxLength(MAX_LENGTH_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH:
				setMinLength(MIN_LENGTH_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE:
				setNullable(NULLABLE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE:
				setMaxValue(MAX_VALUE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE:
				setMinValue(MIN_VALUE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION:
				setRegularExpression(REGULAR_EXPRESSION_EDEFAULT);
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
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE:
				return futureDate != FUTURE_DATE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE:
				return pastDate != PAST_DATE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH:
				return maxLength != null;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH:
				return minLength != null;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE:
				return nullable != NULLABLE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE:
				return maxValue != null;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE:
				return minValue != null;
			case DomainPackage.DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION:
				return regularExpression != null;
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
		result.append(" (futureDate: ");
		result.append(futureDate);
		result.append(", pastDate: ");
		result.append(pastDate);
		result.append(", maxLength: ");
		result.append(maxLength);
		result.append(", minLength: ");
		result.append(minLength);
		result.append(", nullable: ");
		result.append(nullable);
		result.append(", maxValue: ");
		result.append(maxValue);
		result.append(", minValue: ");
		result.append(minValue);
		result.append(", regularExpression: ");
		result.append(regularExpression);
		result.append(')');

		return result.toString();
	}

}
